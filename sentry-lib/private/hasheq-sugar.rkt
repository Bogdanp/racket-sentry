#lang racket/base

(require (for-syntax racket/base
                     syntax/parse/pre))

(provide
 (rename-out
  [hash-#%app #%app]))

(begin-for-syntax
  (define-syntax-class key
    #:literals (unquote)
    (pattern (unquote e:expr) #:with key #'e)
    (pattern e #:with key #''e))

  (define (syntax-property* stx prop)
    (define v (syntax-property stx prop))
    (if (pair? v)
        (car v)
        v)))

(define-syntax (hash-#%app stx)
  (syntax-parse stx
    [{_ {~seq k:key v:expr} ...}
     #:when (equal? (syntax-property* stx 'paren-shape) #\{)
     #:with (e ...) (for*/list ([pair-stx (in-list (syntax-e #'((k.key v) ...)))]
                                [stx (in-list (syntax-e pair-stx))])
                      stx)
     #'(hasheq e ...)]
    [(_ rator . rands)
     #'(#%app rator . rands)]))
