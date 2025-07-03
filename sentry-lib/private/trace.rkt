#lang racket/base

(require "hasheq-sugar.rkt"
         "span.rkt"
         "transaction.rkt")

(provide
 get-trace-context)

(define (get-trace-context [t (current-transaction)])
  (for*/hasheq ([(key accessor) (in-hash trace-context-accessors)]
                [value (in-value (accessor t))]
                #:when value)
    (values key value)))

(define trace-context-accessors
  {op (compose1 symbol->string span-operation)
   description span-description
   trace_id span-trace-id
   span_id span-id
   parent_span_id span-parent-id
   status (compose1 symbol->string span-status)
   origin (compose1 symbol->string span-origin)
   data span-data})

;; Local variables:
;; racket-indent-sequence-depth: 1
;; End:
