#lang racket/base

(require actor
         data/monocle
         net/http-easy
         net/url
         racket/contract/base
         racket/format
         racket/list
         racket/match
         racket/promise
         racket/string
         threading
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

(struct sentry (release environment custodian dispatcher)
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
  (define dispatcher
    (parameterize ([current-custodian custodian])
      (define recv
        (make-log-receiver
         (current-logger)
         'warning))
      (define sess (make-session))
      (define heads
        (hasheq
         'user-agent lib-version
         'x-sentry-auth auth))
      (define timeouts
        (make-timeout-config
         #:connect (/ connect-timeout 1000)
         #:request (/ send-timeout 1000)))
      (dispatch-actor sess heads endpoint backlog timeouts max-breadcrumbs recv)))
  (sentry release environment custodian dispatcher))

(define (sentry-stop [s (current-sentry)])
  (stop (sentry-dispatcher s))
  (sync (actor-dead-evt (sentry-dispatcher s)))
  (custodian-shutdown-all (sentry-custodian s)))

(define sentry-capture-exception!
  (make-keyword-procedure
   (lambda (kws kw-args err [s (current-sentry)] . args)
     (if s
         (send
          (sentry-dispatcher s)
          (let ([e (keyword-apply make-event kws kw-args (cons err args))])
            (struct-copy
             event e
             [environment (or (event-environment e) (sentry-environment s))]
             [release (or (event-release e) (sentry-release s))])))
         (delay/sync (void))))))


;; actor ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-actor (dispatch-actor sess heads endpoint backlog timeouts max-breadcrumbs log-receiver)
  #:state (make-state)
  #:event
  (lambda (st)
    (apply
     choice-evt
     (wrap-evt
      log-receiver
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
         (state-add-breadcrumb st crumb max-breadcrumbs)]))
     (for/list ([promise (in-list (state-pending st))])
       (wrap-evt promise (λ (_) (state-handle-response st promise))))))
  #:stopped?
  (lambda (st)
    (and (state-stopped? st)
         (null? (state-pending st))))
  #:on-stop
  (lambda (_st)
    (session-close! sess))

  (define (send st e)
    (cond
      [(state-stopped? st)
       (log-sentry-warning "dropping event: stopped")
       (values st (delay/sync (void)))]

      [(< (current-inexact-monotonic-milliseconds)
          (state-rate-limit-deadline st))
       (log-sentry-warning "dropping event: rate limited")
       (values st (delay/sync (void)))]

      [(>= (length (state-pending st)) backlog)
       (log-sentry-warning "dropping event: queue full")
       (values st (delay/sync (void)))]

      [else
       (define crumbs
         (state-breadcrumbs st))
       (define promise
         (let ([e (event-attach-breadcrumbs e crumbs)])
           (delay/thread
            (post
             #:data (gzip-payload (json-payload (event->jsexpr e)))
             #:headers heads
             #:timeouts timeouts
             endpoint))))
       (values
        (~> (state-remove-breadcrumbs st)
            (state-add-pending promise))
        (delay/thread
         (sync promise)))]))

  (define (stop st)
    (values
     (&state-stopped? st #t)
     (void))))

(struct state (stopped? pending breadcrumbs rate-limit-deadline))
(define-struct-lenses state)

(define (make-state)
  (state
   #;stopped? #f
   #;pending null
   #;breadcrumbs null
   #;rate-limit-deadline 0))

(define (state-add-breadcrumb st crumb max-crumbs)
  (lens-update
   &state-breadcrumbs st
   (lambda (crumbs)
     (define next-crumbs
       (cons crumb crumbs))
     (if (< (length next-crumbs) max-crumbs)
         next-crumbs
         (take next-crumbs max-crumbs)))))

(define (state-remove-breadcrumbs st)
  (&state-breadcrumbs st null))

(define (state-add-pending st promise)
  (lens-update
   &state-pending st
   (lambda (promises)
     (cons promise promises))))

(define (state-remove-pending st promise)
  (lens-update
   &state-pending st
   (lambda (promises)
     (remq promise promises))))

(define (state-rate-limit st deadline)
  (lens-update
   &state-rate-limit-deadline st
   (lambda (old-deadline)
     (max old-deadline deadline))))

(define (state-handle-response st promise)
  (with-handlers ([exn:fail?
                   (lambda (e)
                     (log-sentry-error
                      "request failed~n error: ~a"
                      (exn-message e))
                     (state-remove-pending st promise))])
    (define res
      (force promise))
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
       (define deadline
         (+ (current-inexact-monotonic-milliseconds)
            (* retry-after 1000)))
       (log-sentry-warning "dropping all events for the next ~a seconds" retry-after)
       (~> (state-rate-limit st deadline)
           (state-remove-pending promise))]

      [(response #:status-code 200)
       (log-sentry-debug "event captured successfully")
       (state-remove-pending st promise)]

      [(response #:status-code status #:headers () headers #:body data)
       (log-sentry-warning "unexpected response from Sentry~n  status: ~.s~n  headers: ~.s~n  data ~.s" status headers data)
       (state-remove-pending st promise)])))
