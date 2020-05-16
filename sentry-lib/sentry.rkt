#lang racket/base

(require json
         net/http-client
         net/url
         racket/async-channel
         racket/contract
         racket/format
         racket/list
         racket/match
         racket/port
         racket/string
         "private/event.rkt"
         "private/http.rkt"
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
                              #:send-timeout-ms [send-timeout 5000])
  (->* (string?)
       (#:backlog exact-positive-integer?
        #:release (or/c false/c non-empty-string?)
        #:environment (or/c false/c non-empty-string?)
        #:send-timeout-ms exact-positive-integer?)
       sentry?)
  (define dsn (string->url dsn:str))
  (define auth (dsn->auth dsn))
  (define endpoint (dsn->endpoint dsn))
  (define custodian (make-custodian))
  (parameterize ([current-custodian custodian])
    (define chan (make-async-channel backlog))
    (define dispatcher (make-sentry-dispatcher chan auth endpoint send-timeout))
    (sentry release environment custodian chan dispatcher)))

(define/contract (sentry-stop [s (current-sentry)])
  (->* () (sentry?) void?)
  (async-channel-put (sentry-chan s) 'stop)
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

(define (make-sentry-dispatcher chan auth endpoint send-timeout)
  (define endpoint:url (string->url endpoint))
  (define conn (http-conn))

  (define (connect!)
    (with-handlers ([exn:fail?
                     (lambda (e)
                       (log-sentry-error "failed to connect: ~a" (exn-message e)))])
      (http-conn-open! conn
                       (url-host endpoint:url)
                       #:port (url-port/safe endpoint:url)
                       #:ssl? (url-ssl? endpoint:url)
                       #:auto-reconnect? #t)))

  (define (send-event! data)
    (define res (make-channel))
    (define thd
      (thread
       (lambda ()
         (unless (http-conn-live? conn)
           (connect!))

         (let loop ([failures 0])
           (with-handlers ([exn:fail?
                            (lambda (e)
                              (cond
                                [(zero? failures)
                                 (log-sentry-warning "request failed: ~a" (exn-message e))
                                 (connect!)
                                 (loop (add1 failures))]

                                [else
                                 (raise e)]))])
             (call-with-values
              (lambda ()
                (http-conn-sendrecv! conn endpoint
                                     #:method "POST"
                                     #:headers (list "Content-type: application/json; charset=utf-8"
                                                     (~a "User-Agent: racket-sentry/" (lib-version))
                                                     (~a "X-Sentry-Auth: " auth))
                                     #:data data))
              (lambda vs
                (channel-put res vs))))))))

    (sync
     (handle-evt
      (alarm-evt (+ (current-inexact-milliseconds) send-timeout))
      (lambda (_e)
        (kill-thread thd)
        (error 'timeout)))
     (handle-evt
      res
      (lambda (vs)
        (apply values vs)))))

  (define (dispatcher)
    (log-sentry-debug "dispatcher ready for action")
    (let loop ([rate-limit-deadline 0])
      (with-handlers ([exn:fail?
                       (lambda (e)
                         (log-sentry-error "failed to dispatch: ~a" (exn-message e))
                         (loop 0))])
        (match (sync chan)
          ['stop
           (log-sentry-debug "received stop event")
           (http-conn-close! conn)]

          [(and (? event?) e)
           #:when (< (current-inexact-milliseconds) rate-limit-deadline)
           (log-sentry-warning "dropping event ~.s due to rate limit" e)
           (loop rate-limit-deadline)]

          [(and (? event?) e)
           (log-sentry-debug "capturing event ~.s" e)
           (define-values (status headers out)
             (send-event! (jsexpr->bytes (event->jsexpr e))))

           (define data (port->bytes out))
           (log-sentry-debug "received response~n  status: ~.s~n  headers: ~.s~n  data: ~.s" status headers data)
           (match status
             [(regexp #px"HTTP.... 429 ")
              (log-sentry-warning "rate limit reached")
              (define retry-after
                (or
                 (string->number
                  (bytes->string/utf-8
                   (or (headers-ref headers #"retry-after") #"")))
                 15))

              (log-sentry-warning "dropping all events for the next ~a seconds" retry-after)
              (loop (+ (current-inexact-milliseconds) (* retry-after 1000)))]

             [(regexp #px"HTTP.... 200 ")
              (log-sentry-debug "event captured successfully")
              (loop 0)]

             [_
              (log-sentry-warning "unexpected response from Sentry~n  status: ~.s~n  headers: ~.s~n  data ~.s" status headers data)
              (loop 0)])]))))

  (thread dispatcher))

(define sentry-capture-exception!
  (make-keyword-procedure
   (lambda (kws kw-args e [s (current-sentry)] . args)
     (when s
       (define evt
         (let ([evt (keyword-apply make-event kws kw-args (cons e args))])
           (struct-copy event evt
                        [environment (or (event-environment evt) (sentry-environment s))]
                        [release (or (event-release evt) (sentry-release s))])))
       (async-channel-put (sentry-chan s) evt)))))
