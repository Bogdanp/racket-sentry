#lang racket/base

(require rackunit
         sentry
         sentry/cron
         "common.rkt")

(provide
 cron-tests)

(define cron-tests
  (test-suite
   "cron"

   (test-suite
    "call-with-monitor"

    (when test-dsn
      (test-case "can monitor cron jobs"
        (define c
          (make-sentry
           test-dsn
           #:environment "test"
           #:release "0.0.1"))
        (parameterize ([current-sentry c])
          (call-with-monitor
           "example-cron-job"
           #:config
           (monitor-config
            (schedule 'crontab "* * * * *"))
           (lambda ()
             (void))))
        (sentry-stop c))))))

(module+ test
  (require rackunit/text-ui)
  (run-tests cron-tests))
