#lang racket/base

(require (for-syntax racket/base
                     setup/getinfo))

(provide
 lib-version)

(begin-for-syntax
  (define (info-ref id)
    ((get-info '("sentry")) id)))

(define-syntax (get-version stx)
  (datum->syntax stx (info-ref 'version)))

(define lib-version
  (format "racket-sentry/~a" (get-version)))
