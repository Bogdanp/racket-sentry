#lang racket/base

(require rackunit
         "event.rkt"
         "http.rkt"
         "sentry.rkt"
         "user.rkt")

(define all-sentry-tests
  (test-suite
   "sentry"

   event-tests
   http-tests
   sentry-tests
   user-tests))

(module+ main
  (require rackunit/text-ui)
  (run-tests all-sentry-tests))
