#lang racket/base

(require rackunit
         sentry/private/user)

(provide
 user-tests)

(define user-tests
  (test-suite
   "user"

   (test-suite
    "sentry-user->jsexpr"

    (test-case "drops unset values"
      (define u
        (make-sentry-user #:id "10"
                          #:username "bogdan"
                          #:email "bogdan@defn.io"))
      (check-equal? (sentry-user->jsexpr u)
                    (hasheq 'id "10"
                            'username "bogdan"
                            'email "bogdan@defn.io"))))))

(module+ test
  (require rackunit/text-ui)
  (run-tests user-tests))
