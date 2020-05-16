#lang racket/base

(require rackunit
         sentry/private/http)

(provide
 http-tests)

(define http-tests
  (test-suite
   "http"

   (test-suite
    "headers-ref"

    (check-false (headers-ref null #"a"))
    (check-equal? (headers-ref '(#"b: 1" #"a: 2") #"a") #"2")
    (check-equal? (headers-ref '(#"b: 1" #"A: 2") #"a") #"2")
    (check-equal? (headers-ref '(#"b: 1" #"AbC: 2") #"abc") #"2"))))

(module+ test
  (require rackunit/text-ui)
  (run-tests http-tests))
