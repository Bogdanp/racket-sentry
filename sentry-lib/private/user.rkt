#lang racket/base

(require racket/contract
         racket/function
         racket/string)

(provide
 current-sentry-user
 make-sentry-user
 user?
 user->event)

(struct user (id username email ip-address subscription)
  #:transparent)

(define/contract current-sentry-user
  (parameter/c (or/c false/c user?))
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
       user?)
  (user id username email ip-address subscription))

(define OPTIONAL-USER-ACCESSORS
  (hasheq 'username user-username
          'email user-email
          'ip_address user-ip-address
          'subscription user-subscription))

(define (user->event u)
  (for/fold ([h (hasheq 'id (user-id u))])
            ([(key accessor) (in-hash OPTIONAL-USER-ACCESSORS)])
    (cond
      [(accessor u) => (curry hash-set h key)]
      [else h])))
