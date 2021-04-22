#lang racket/base

(require rackunit
         "event.rkt"
         "sentry.rkt"
         "user.rkt")

(define all-sentry-tests
  (test-suite
   "sentry"

   event-tests
   sentry-tests
   user-tests))

(module+ main
  (require rackunit/text-ui)
  (run-tests all-sentry-tests))
