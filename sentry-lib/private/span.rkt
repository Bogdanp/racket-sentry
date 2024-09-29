#lang racket/base

(require "date.rkt"
         "hasheq-sugar.rkt"
         "random.rkt")

(provide
 (struct-out span)
 make-span
 span-finalize!
 span-set!
 span->jsexpr
 current-span)

(define current-span
  (make-parameter #f))

(struct span
  (id
   trace-id
   parent-id
   operation
   description
   start-timestamp
   [end-timestamp #:mutable]
   [status #:mutable]
   origin
   data))

(define (make-span #:operation [operation 'function]
                   #:description [description #f]
                   #:origin [origin 'manual]
                   #:data [data #f])
  (define parent
    (current-span))
  (span
   #;id (generate-random-id 8)
   #;trace-id (and parent (span-trace-id parent))
   #;parent-id (and parent (span-id parent))
   #;operation operation
   #;description description
   #;start-timestamp (current-seconds*)
   #;end-timestamp #f
   #;status 'ok
   #;origin origin
   #;data (if data
              (hash-copy data)
              (make-hasheq))))

(define (span-finalize! s)
  (set-span-end-timestamp! s (current-seconds*)))

(define (span-set! s k v)
  (hash-set! (span-data s) k v))

(define (span->jsexpr s)
  (for*/hasheq ([(key accessor) (in-hash accessors)]
                [value (in-value (accessor s))]
                #:when value)
    (values key value)))

(define accessors
  {span_id span-id
   trace_id span-trace-id
   parent_span_id span-parent-id
   op (compose1 symbol->string span-operation)
   description span-description
   start_timestamp span-start-timestamp
   timestamp span-end-timestamp
   status (compose1 symbol->string span-status)
   data span-data
   origin (compose1 symbol->string span-origin)})

;; Local variables:
;; racket-indent-sequence-depth: 1
;; End:
