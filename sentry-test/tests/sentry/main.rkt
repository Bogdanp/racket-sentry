#lang racket/base

(require rackunit
         "event.rkt"
         "sentry.rkt")

(define all-sentry-tests
  (test-suite
   "sentry"

   event-tests
   sentry-tests))

(module+ main
  (require rackunit/text-ui)
  (run-tests all-sentry-tests))
