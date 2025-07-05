#lang racket/base

(require box-extra
         racket/hash
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
   request
   source
   spans-box
   update-spans-box))

(define current-transaction
  (make-parameter #f))

(define (make-transaction name
                          #:data [data #f]
                          #:origin [origin 'manual]
                          #:source [source 'custom]
                          #:trace-id [trace-id #f]
                          #:parent-id [parent-id #f]
                          #:operation [operation 'function]
                          #:description [description #f]
                          #:environment [environment #f]
                          #:release [release #f]
                          #:request [request #f])
  (define parent
    (current-transaction))
  (define spans-box
    (box null))
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
   #;origin origin
   #;data (if data
              (hash-copy data)
              (make-hasheq))
   #;name name
   #;user #f
   #;environment environment
   #;release release
   #;request request
   #;source source
   #;spans-box spans-box
   #;update-spans-box (make-box-update-proc spans-box)))

(define (add-transaction-span! t s)
  ((transaction-update-spans-box t)
   (lambda (spans)
     (cons s spans)))
  (void))

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
   spans (λ (t) (map span->jsexpr (unbox (transaction-spans-box t))))
   user (λ-and~>
         transaction-user
         sentry-user->jsexpr)
   environment transaction-environment
   release transaction-release
   request transaction-request})

;; Local variables:
;; racket-indent-sequence-depth: 1
;; End:
