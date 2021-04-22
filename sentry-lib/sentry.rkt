#lang racket/base

(require net/http-easy
         net/url
         racket/async-channel
         racket/contract
         racket/format
         racket/list
         racket/match
         racket/port
         racket/string
         "private/event.rkt"
         "private/reflect.rkt")

(provide
 current-sentry
 make-sentry
 sentry?
 sentry-capture-exception!
 sentry-stop)

(define-logger sentry)

(struct sentry (release environment custodian chan dispatcher)
  #:transparent)

(define/contract current-sentry
  (parameter/c (or/c false/c sentry?))
  (make-parameter #f))

(define/contract (make-sentry dsn:str
                              #:backlog [backlog 128]
                              #:release [release (getenv "SENTRY_RELEASE")]
                              #:environment [environment (getenv "SENTRY_ENVIRONMENT")]
                              #:connect-timeout-ms [connect-timeout 5000]
                              #:send-timeout-ms [send-timeout 5000])
  (->* (string?)
       (#:backlog exact-positive-integer?
        #:release (or/c false/c non-empty-string?)
        #:environment (or/c false/c non-empty-string?)
        #:connect-timeout-ms exact-positive-integer?
        #:send-timeout-ms exact-positive-integer?)
       sentry?)
  (define dsn (string->url dsn:str))
  (define auth (dsn->auth dsn))
  (define endpoint (dsn->endpoint dsn))
  (define custodian (make-custodian))
  (parameterize ([current-custodian custodian])
    (define chan (make-async-channel backlog))
    (define timeouts
      (make-timeout-config
       #:connect (/ connect-timeout 1000)
       #:request (/ send-timeout 1000)))
    (define dispatcher (make-sentry-dispatcher chan auth endpoint timeouts))
    (sentry release environment custodian chan dispatcher)))

(define/contract (sentry-stop [s (current-sentry)])
  (->* () (sentry?) void?)
  (async-channel-put (sentry-chan s) '(stop))
  (thread-wait (sentry-dispatcher s))
  (custodian-shutdown-all (sentry-custodian s)))

(define (dsn->auth dsn)
  (with-output-to-bytes
    (lambda ()
      (define-values (key secret)
        (match (string-split (or (url-user dsn) "") ":")
          [(list key)        (values key #f)]
          [(list key secret) (values key secret)]))

      (printf "Sentry sentry_version=7, sentry_client=racket-sentry/~a, " (lib-version))
      (printf "sentry_timestamp=~a, " (current-seconds))
      (printf "sentry_key=~a" key)
      (when secret
        (printf ", sentry_secret=~a" secret)))))

(define (dsn->endpoint dsn)
  (define-values (path project-id)
    (split-at-right (url-path dsn) 1))

  (~a (url-scheme dsn)
      "://"
      (url-host dsn)
      ":"
      (url-port/safe dsn)
      (url-path->string path)
      "/api/"
      (url-path->string project-id)
      "/store/"))

(define (url-path->string p)
  (string-join (map path/param-path p) "/"))

(define (url-port/safe u)
  (or (url-port u)
      (case (url-scheme u)
        [("https") 443]
        [else      80])))

(define (make-sentry-dispatcher chan auth endpoint timeouts)
  (define sess (make-session))
  (define heads
    (hasheq
     'user-agent (~a "racket-sentry/" (lib-version))
     'x-sentry-auth auth))
  (parameterize ([current-session sess])
    (define (dispatcher)
      (log-sentry-debug "dispatcher ready for action")
      (let loop ([rate-limit-deadline 0])
        (with-handlers ([exn:fail?
                         (lambda (e)
                           (log-sentry-error "dispatch failed: ~a" (exn-message e))
                           (loop 0))])
          (match (sync chan)
            ['(stop)
             (log-sentry-debug "received stop event")
             (session-close! sess)]

            [(? event? e)
             #:when (< (current-inexact-milliseconds) rate-limit-deadline)
             (log-sentry-warning "dropping event ~.s due to rate limit" e)
             (loop rate-limit-deadline)]

            [(? event? e)
             (log-sentry-debug "capturing event ~.s" e)
             (define res
               (post endpoint
                     #:data (gzip-payload (json-payload (event->jsexpr e)))
                     #:headers heads
                     #:timeouts timeouts))

             (log-sentry-debug
              "received response~n  status: ~.s~n  headers: ~.s~n  data: ~.s"
              (response-status-code res)
              (response-headers res)
              (response-body res))
             (match res
               [(response #:status-code 429)
                (log-sentry-warning "rate limit reached")
                (define retry-after
                  (or
                   (string->number
                    (bytes->string/utf-8
                     (or (response-headers-ref res 'retry-after) #"")))
                   15))

                (log-sentry-warning "dropping all events for the next ~a seconds" retry-after)
                (loop (+ (current-inexact-milliseconds) (* retry-after 1000)))]

               [(response #:status-code 200)
                (log-sentry-debug "event captured successfully")
                (loop 0)]

               [(response #:status-code status #:headers () headers #:body data)
                (log-sentry-warning "unexpected response from Sentry~n  status: ~.s~n  headers: ~.s~n  data ~.s" status headers data)
                (loop 0)])]))))

    (thread dispatcher)))

(define sentry-capture-exception!
  (make-keyword-procedure
   (lambda (kws kw-args e [s (current-sentry)] . args)
     (when s
       (define ch (sentry-chan s))
       (define evt
         (let ([evt (keyword-apply make-event kws kw-args (cons e args))])
           (struct-copy event evt
                        [environment (or (event-environment evt) (sentry-environment s))]
                        [release (or (event-release evt) (sentry-release s))])))

       (unless (sync/timeout 0 (async-channel-put-evt ch evt))
         (log-sentry-debug "dropping event ~.s because the backlog is full" evt))))))
