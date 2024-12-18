#lang racket/base

(require (for-syntax racket/base)
         racket/class
         racket/contract/base
         sentry/tracing
         (only-in db/private/generic/interfaces
                  connection<%>
                  connection?
                  statement-binding?
                  statement-binding-pst))

(provide
 traced-connection%
 (contract-out
  [trace-connection (-> connection? connection?)]))

(define-syntax-rule (proxy target-id (meth-id arg-id ...) ...)
  (begin
    (define/public (meth-id arg-id ...)
      (send target-id meth-id arg-id ...)) ...))

(define traced-connection%
  (class* object% (connection<%>)
    (init-field base)
    (super-new)

    (define/public (get-base)
      base)

    (define/public (query fsym stmt cursor?)
      (define the-stmt
        (if (statement-binding? stmt)
            (send (statement-binding-pst stmt) get-stmt)
            stmt))
      (call-with-span
       #:origin 'auto.db
       #:operation 'db.query
       #:description (format "~a" the-stmt)
       (lambda (_)
         (send base query fsym stmt cursor?))))

    (proxy
     base
     [connected?]
     [disconnect]
     [get-dbsystem]
     [prepare fsym stmt close-on-exec?]
     [fetch/cursor fsym cursor fetch-size]
     [list-tables fsym schema]
     [start-transaction fsym isolation option cwt?]
     [end-transaction fsym mode cwt?]
     [transaction-status fsym]
     [free-statement pst need-lock?])))

(define (trace-connection conn)
  (new traced-connection% [base conn]))
