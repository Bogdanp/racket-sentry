#lang racket/base

(require racket/contract/base
         racket/string
         "hasheq-sugar.rkt")

(provide
 sentry-user?
 sentry-user->jsexpr
 (contract-out
  [current-sentry-user
   (parameter/c (or/c #f sentry-user?))]
  [make-sentry-user
   (->* [#:id non-empty-string?]
        [#:username (or/c #f non-empty-string?)
         #:email (or/c #f non-empty-string?)
         #:ip-address (or/c #f non-empty-string?)
         #:subscription (or/c #f non-empty-string?)]
        sentry-user?)]))

(struct sentry-user (id username email ip-address subscription)
  #:transparent)

(define current-sentry-user
  (make-parameter #f))

(define (make-sentry-user #:id id
                          #:username [username #f]
                          #:email [email #f]
                          #:ip-address [ip-address #f]
                          #:subscription [subscription #f])
  (sentry-user id username email ip-address subscription))

(define accessors
  {id sentry-user-id
   username sentry-user-username
   email sentry-user-email
   ip_address sentry-user-ip-address
   subscription sentry-user-subscription})

(define (sentry-user->jsexpr u)
  (for*/hasheq ([(key accessor) (in-hash accessors)]
                [value (in-value (accessor u))]
                #:when value)
    (values key value)))

;; Local variables:
;; racket-indent-sequence-depth: 1
;; End:
