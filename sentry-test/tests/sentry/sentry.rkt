#lang racket/base

(require rackunit
         sentry
         "common.rkt")

(provide
 sentry-tests)

(define sentry-tests
  (test-suite
   "sentry"

   (when test-dsn
     (let ([client (make-sentry test-dsn
                                #:environment "test"
                                #:release "0.0.1")])
       (test-suite
        "sentry-capture-exception!"

        (test-case "can capture exceptions"
          (define e (make-exn:fail "an exception" (current-continuation-marks)))
          (sentry-capture-exception! e client))

        (test-case "can capture exceptions using the current-sentry"
          (parameterize ([current-sentry client])
            (define e (make-exn:fail "an exception" (current-continuation-marks)))
            (sentry-capture-exception! e))))))))

(module+ test
  (require rackunit/text-ui)
  (run-tests sentry-tests))
