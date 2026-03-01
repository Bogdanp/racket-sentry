#lang racket/base

(require racket/contract/base
         racket/promise
         racket/string
         threading
         "private/check-in.rkt"
         "private/date.rkt"
         "private/random.rkt"
         (only-in "sentry.rkt" current-sentry)
         (submod "sentry.rkt" private))

(provide
 (contract-out
  [struct monitor-config ;; noqa
    ([schedule schedule?])]
  [struct schedule ;; noqa
    ([type (or/c 'crontab 'interval)]
     [value string?])]
  [call-with-monitor
   (->* [non-empty-string? (-> any)]
        [#:config (or/c #f monitor-config?)]
        any)]))

(define (call-with-monitor
         #:config [config #f]
         slug proc)
  (define-values (id _evt)
    (capture-check-in!
     #:status 'in-progress
     slug config))
  (define t0 (current-seconds*))
  (with-handlers ([exn:fail?
                   (lambda (e)
                     (define duration (- (current-seconds*) t0))
                     (capture-check-in!
                      #:id id
                      #:status 'error
                      #:duration duration
                      slug)
                     (raise e))])
    (define result (proc))
    (define duration (- (current-seconds*) t0))
    (capture-check-in!
     #:id id
     #:status 'ok
     #:duration duration
     slug)
    result))

(define (capture-check-in!
         #:id [id (generate-random-id)]
         #:status [status 'in-progress]
         #:duration [duration #f]
         monitor-slug
         [monitor-config #f]
         [sentry (current-sentry)])
  (define check-in-event
    (make-check-in
     #:id id
     #:status status
     #:duration duration
     #:environment (and~> sentry sentry-environment)
     #:release (and~> sentry sentry-release)
     monitor-slug monitor-config))
  (define evt
    (if sentry
        (capture
         (sentry-dispatcher sentry)
         check-in-event)
        (delay/sync (void))))
  (values id evt))
