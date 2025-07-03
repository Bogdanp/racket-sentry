#lang racket/base

(require racket/hash
         racket/lazy-require
         threading
         "context.rkt"
         "date.rkt"
         "hasheq-sugar.rkt"
         "random.rkt"
         "span.rkt"
         "user.rkt")

(lazy-require
 ["trace.rkt" (get-trace-context)])

(provide
 (struct-out transaction)
 make-transaction
 current-transaction
 transaction->jsexpr
 transaction-finalize!
 add-transaction-span!)

(struct transaction span
  (name
   [user #:mutable]
   environment
   release
   source
   spans-mu
   [spans #:mutable]))

(define current-transaction
  (make-parameter #f))

(define (make-transaction name
                          #:data [data #f]
                          #:source [source 'custom]
                          #:trace-id [trace-id #f]
                          #:parent-id [parent-id #f]
                          #:operation [operation 'function]
                          #:description [description #f]
                          #:environment [environment #f]
                          #:release [release #f])
  (define parent
    (current-transaction))
  (transaction
   #;id (generate-random-id 8)
   #;trace-id (cond
                [trace-id]
                [parent (span-trace-id parent)]
                [else (generate-random-id)])
   #;parent-id (or parent-id (and parent (span-id parent)))
   #;operation (or operation 'function)
   #;description description
   #;start-timestamp (current-seconds*)
   #;end-timestamp #f
   #;status 'ok
   #;origin #f
   #;data (if data
              (hash-copy data)
              (make-hasheq))
   #;name name
   #;user #f
   #;environment environment
   #;release release
   #;source source
   #;spans-mu (make-semaphore 1)
   #;spans null))

(define (add-transaction-span! t s)
  (call-with-semaphore (transaction-spans-mu t)
    (lambda ()
      (set-transaction-spans! t (cons s (transaction-spans t))))))

(define (transaction-finalize! t)
  (set-transaction-user! t (current-sentry-user))
  (span-finalize! t))

(define (transaction->jsexpr t)
  (for*/hasheq ([(key accessor) (in-hash accessors)]
                [value (in-value (accessor t))]
                #:when value)
    (values key value)))

(define accessors
  {type (λ (_) "transaction")
   transaction transaction-name
   transaction_info (λ (t) {source (symbol->string (transaction-source t))})
   start_timestamp span-start-timestamp
   timestamp span-end-timestamp
   contexts (λ (t)
              (hash-union
               (get-common-contexts)
               {trace (get-trace-context t)}))
   spans (λ (t) (map span->jsexpr (transaction-spans t)))
   user (λ-and~>
         transaction-user
         sentry-user->jsexpr)
   environment transaction-environment
   release transaction-release})

;; Local variables:
;; racket-indent-sequence-depth: 1
;; End:
