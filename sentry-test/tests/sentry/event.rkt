#lang racket/base

(require gregor
         rackunit
         sentry/private/event
         threading)

(provide
 event-tests)

(define event-tests
  (test-suite
   "event"

   (test-suite
    "event->jsexpr"

    (test-case "converts events to json expressions"
      (parameterize ([current-clock (lambda () 0)]
                     [current-timezone "UTC"])
        (define expr
          (event->jsexpr (make-event (make-exn:fail "an error" (current-continuation-marks)))))

        (check-equal?
         (~> expr
             (hash-remove 'exception)
             (hash-remove 'contexts))
         (hasheq 'platform "other"
                 'level "error"
                 'timestamp "1970-01-01T00:00:00Z"
                 'tags (hash))))))))

(module+ test
  (require rackunit/text-ui)
  (run-tests event-tests))
