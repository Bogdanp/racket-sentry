#lang racket/base

(require json
         racket/contract/base
         threading
         "private/span.rkt"
         "private/transaction.rkt"
         (only-in "sentry.rkt" current-sentry)
         (submod "sentry.rkt" private))

(provide
 (contract-out
  [transaction? (-> any/c boolean?)]
  [transaction-name (-> transaction? string?)]
  [current-transaction (parameter/c transaction?)]
  [call-with-transaction
   (->* [string? (-> transaction? any)]
        [#:data (or/c #f (hash/c symbol? jsexpr?))
         #:source symbol?
         #:trace-id (or/c #f string?)
         #:parent-id (or/c #f string?)
         #:operation symbol?
         #:description (or/c #f string?)]
        any)]
  [span? (-> any/c boolean?)]
  [current-span (parameter/c span?)]
  [call-with-span
   (->* [(-> span? any)]
        [#:operation symbol?
         #:description (or/c #f string?)
         #:origin symbol?
         #:data (or/c #f (hash/c symbol? jsexpr?))]
        any)]
  [span-set! (-> span? symbol? jsexpr? void?)]
  [rename span-id get-span-id (-> span? string?)]
  [rename span-trace-id get-trace-id (-> span? string?)]
  [rename span-operation get-span-operation (-> span? symbol?)]
  [rename span-data get-span-data (-> span? (or/c #f (hash/c symbol? jsexpr?)))]))

(define (call-with-transaction
          #:data [data #f]
          #:source [source 'custom]
          #:trace-id [trace-id #f]
          #:parent-id [parent-id #f]
          #:operation [operation 'function]
          #:description [description #f]
          name proc)
  (define t #f)
  (dynamic-wind
    (lambda ()
      (define c
        (current-sentry))
      (set! t (make-transaction
               #:data data
               #:source source
               #:trace-id trace-id
               #:parent-id parent-id
               #:operation operation
               #:description description
               #:environment (and~> c sentry-environment)
               #:release (and~> c sentry-release)
               name)))
    (lambda ()
      (parameterize ([current-transaction t]
                     [current-span t])
        (proc t)))
    (lambda ()
      (transaction-finalize! t)
      (define c (current-sentry))
      (when c (capture (sentry-dispatcher c) t)))))

(define (call-with-span
         #:operation [operation 'function]
         #:description [description #f]
         #:origin [origin 'manual]
         #:data [data #f]
         proc)
  (define s #f)
  (dynamic-wind
    (lambda ()
      (set! s (make-span
               #:operation operation
               #:description description
               #:origin origin
               #:data data)))
    (lambda ()
      (parameterize ([current-span s])
        (proc s)))
    (lambda ()
      (span-finalize! s)
      (define t (current-transaction))
      (when t (add-transaction-span! t s)))))
