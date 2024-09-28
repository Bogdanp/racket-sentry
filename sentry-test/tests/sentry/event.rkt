#lang racket/base

(require rackunit
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
      (define expr
        (event->jsexpr
         (make-event
          #:timestamp (seconds->date 0 #f)
          (make-exn:fail "an error" (current-continuation-marks)))))

      (check-equal?
       (~> expr
           (hash-remove 'exception)
           (hash-remove 'contexts))
       (hasheq 'platform "other"
               'level "error"
               'timestamp "1970-01-01T00:00:00.000000000Z"
               'tags (hash)))))))

(module+ test
  (require rackunit/text-ui)
  (run-tests event-tests))
