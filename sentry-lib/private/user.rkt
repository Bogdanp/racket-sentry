#lang racket/base

(require racket/contract
         racket/string)

(provide
 current-sentry-user
 make-sentry-user
 sentry-user?
 sentry-user->jsexpr)

(struct sentry-user (id username email ip-address subscription)
  #:transparent)

(define/contract current-sentry-user
  (parameter/c (or/c false/c sentry-user?))
  (make-parameter #f))

(define/contract (make-sentry-user #:id id
                                   #:username [username #f]
                                   #:email [email #f]
                                   #:ip-address [ip-address #f]
                                   #:subscription [subscription #f])
  (->* (#:id non-empty-string?)
       (#:username (or/c false/c non-empty-string?)
        #:email (or/c false/c non-empty-string?)
        #:ip-address (or/c false/c non-empty-string?)
        #:subscription (or/c false/c non-empty-string?))
       sentry-user?)
  (sentry-user id username email ip-address subscription))

(define accessors
  (hasheq 'id sentry-user-id
          'username sentry-user-username
          'email sentry-user-email
          'ip_address sentry-user-ip-address
          'subscription sentry-user-subscription))

(define (sentry-user->jsexpr u)
  (for*/hasheq ([(key accessor) (in-hash accessors)]
                [value (in-value (accessor u))]
                #:when value)
    (values key value)))

(module+ test
  (require rackunit))
