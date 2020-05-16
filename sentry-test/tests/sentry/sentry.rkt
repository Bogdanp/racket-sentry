#lang racket/base

(require racket/async-channel
         rackunit
         sentry
         web-server/http
         web-server/servlet-dispatch
         web-server/web-server
         "common.rkt")

(provide
 sentry-tests)

(define e (make-exn:fail "an exception" (current-continuation-marks)))
(define sentry-tests
  (test-suite
   "sentry"

   (test-suite
    "sentry-capture-exception!"

    (when test-dsn
      (test-case "can capture exceptions"
        (define c (make-sentry test-dsn
                               #:environment "test"
                               #:release "0.0.1"))
        (sentry-capture-exception! e c)
        (sentry-stop c)))

    (when test-dsn
      (test-case "can capture exceptions using the current-sentry"
        (parameterize ([current-sentry (make-sentry test-dsn
                                                    #:environment "test"
                                                    #:release "0.0.1")])
          (sentry-capture-exception! e)
          (sentry-stop))))

    (test-case "gives up sending exceptions if Sentry is unavailable"
      (define c (make-sentry "https://example@example.com/example"))
      (sentry-capture-exception! e c)
      (sentry-stop c))

    (let ([stop #f]
          [total 0])
      (test-suite
       "retries"

       #:before
       (lambda ()
         (define ch (make-async-channel 10))
         (set! stop (serve
                     #:confirmation-channel ch
                     #:port 9095
                     #:dispatch (dispatch/servlet
                                 (lambda (_req)
                                   (set! total (add1 total))
                                   (response/empty
                                    #:code 429
                                    #:message #"Too Many Requests"
                                    #:headers (list (make-header #"Retry-After" #"1")))))))
         (sync ch))

       #:after
       (lambda ()
         (stop))

       (test-case "respects rate limits"
         (parameterize ([current-sentry (make-sentry "http://test@127.0.0.1:9095/test")])
           (for ([_ (in-range 10)])
             (sentry-capture-exception! e))
           (sleep 1)
           (sentry-capture-exception! e)
           (sentry-stop)
           (check-equal? total 2))))))))

(module+ test
  (require rackunit/text-ui)
  (run-tests sentry-tests))
