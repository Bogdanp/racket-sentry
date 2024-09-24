#lang racket/base

(require net/http-easy
         net/url
         racket/async-channel
         racket/contract/base
         racket/format
         racket/list
         racket/match
         racket/string
         "private/dsn.rkt"
         "private/event.rkt"
         "private/reflect.rkt")

(provide
 sentry?
 sentry-capture-exception!
 (contract-out
  [current-sentry
   (parameter/c (or/c #f sentry?))]
  [make-sentry
   (->* [string?]
        [#:backlog exact-positive-integer?
         #:release (or/c #f non-empty-string?)
         #:environment (or/c #f non-empty-string?)
         #:connect-timeout-ms exact-positive-integer?
         #:send-timeout-ms exact-positive-integer?
         #:max-breadcrumbs exact-positive-integer?]
        sentry?)]
  [sentry-stop
   (->* [] [sentry?] void?)]))

(define-logger sentry)

(struct sentry (release environment custodian chan dispatcher)
  #:transparent)

(define current-sentry
  (make-parameter #f))

(define (make-sentry dsn:str
                     #:backlog [backlog 128]
                     #:release [release (getenv "SENTRY_RELEASE")]
                     #:environment [environment (getenv "SENTRY_ENVIRONMENT")]
                     #:connect-timeout-ms [connect-timeout 5000]
                     #:send-timeout-ms [send-timeout 5000]
                     #:max-breadcrumbs [max-breadcrumbs 50])
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
    (define dispatcher (make-sentry-dispatcher chan auth endpoint timeouts max-breadcrumbs))
    (sentry release environment custodian chan dispatcher)))

(define (sentry-stop [s (current-sentry)])
  (async-channel-put (sentry-chan s) '(stop))
  (thread-wait (sentry-dispatcher s))
  (custodian-shutdown-all (sentry-custodian s)))

(define (make-sentry-dispatcher chan auth endpoint timeouts max-breadcrumbs)
  (define recv (make-log-receiver (current-logger) 'warning))
  (define sess (make-session))
  (define heads
    (hasheq
     'user-agent lib-version
     'x-sentry-auth auth))
  (parameterize ([current-session sess])
    (define (dispatcher)
      (log-sentry-debug "dispatcher ready for action")
      (with-handlers* ([exn:fail?
                        (lambda (e)
                          (log-sentry-error "dispatch failed: ~a" (exn-message e))
                          (dispatcher))])
        (let loop ([rate-limit-deadline 0]
                   [breadcrumbs null])
          (sync
           (handle-evt
            recv
            (match-lambda
              [(vector level message value topic)
               (define crumb
                 (make-breadcrumb
                  #:category 'log
                  #:message message
                  #:level level
                  #:data (hasheq
                          'topic (~a topic)
                          'value (~s value))))
               (define breadcrumbs*
                 (cons crumb breadcrumbs))
               (loop rate-limit-deadline
                     (cond
                       [(< (length breadcrumbs*) max-breadcrumbs) breadcrumbs*]
                       [else (take breadcrumbs* max-breadcrumbs)]))]))

           (handle-evt
            chan
            (match-lambda
              ['(stop)
               (log-sentry-debug "received stop event")
               (session-close! sess)]

              [(? event? e)
               #:when (< (current-inexact-milliseconds) rate-limit-deadline)
               (log-sentry-warning "dropping event ~.s due to rate limit" e)
               (loop rate-limit-deadline breadcrumbs)]

              [(? event? e)
               (log-sentry-debug "capturing event ~.s" e)
               (define res
                 (post endpoint
                       #:data (gzip-payload (json-payload (event->jsexpr (event-attach-breadcrumbs e breadcrumbs))))
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
                  (loop (+ (current-inexact-milliseconds) (* retry-after 1000)) null)]

                 [(response #:status-code 200)
                  (log-sentry-debug "event captured successfully")
                  (loop 0 null)]

                 [(response #:status-code status #:headers () headers #:body data)
                  (log-sentry-warning "unexpected response from Sentry~n  status: ~.s~n  headers: ~.s~n  data ~.s" status headers data)
                  (loop 0 null)])]))))))

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
