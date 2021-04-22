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

(define (make-server start)
  (define ch (make-async-channel))
  (begin0 (serve
           #:confirmation-channel ch
           #:port 9095
           #:dispatch (dispatch/servlet start))
    (sync ch)))

(define (box-update! b f)
  (define v (unbox b))
  (unless (box-cas! b v (f v))
    (box-update! b f)))

(define e
  (make-exn:fail "an exception" (current-continuation-marks)))

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
        (sentry-stop c))

      (test-case "can capture exceptions with breadcrumbs"
        (define c (make-sentry test-dsn
                               #:environment "test"
                               #:release "0.0.1"))
        (log-warning "oh no, something bad is happening!")
        (sentry-capture-exception! (exn:fail "an exception with crumbs" (current-continuation-marks)) c)
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
          [total (box 0)])
      (test-suite
       "retries"

       #:before
       (lambda ()
         (set! stop (make-server
                     (lambda (_req)
                       (box-update! total add1)
                       (response/output
                        #:code 429
                        #:message #"Too Many Requests"
                        #:headers (list (make-header #"Retry-After" #"2"))
                        void)))))

       #:after
       (lambda ()
         (stop))

       (test-case "respects rate limits"
         (parameterize ([current-sentry (make-sentry "http://test@127.0.0.1:9095/test")])
           (for ([_ (in-range 10)])
             (sentry-capture-exception! e))
           (sleep 2)
           (sentry-capture-exception! e)
           (sentry-stop)
           (check-equal? (unbox total) 2)))))

    (let ([stop #f])
      (test-suite
       "timeouts"

       #:before
       (lambda ()
         (set! stop (make-server
                     (lambda (_req)
                       (sleep 5)
                       (response/output void)))))

       #:after
       (lambda ()
         (stop))

       (test-case "times out after `send-timeout' milliseconds"
         (parameterize ([current-sentry (make-sentry
                                         #:send-timeout-ms 1000
                                         "http://test@127.0.0.1:9095/test")])
           (define t0 (current-seconds))
           (sentry-capture-exception! e)
           (sentry-stop)
           (check-true (<= (- (current-seconds) t0) 2))))))

    (let ([stop #f]
          [total (box 0)])
      (test-suite
       "backlog"

       #:before
       (lambda ()
         (set! stop (make-server
                     (lambda (_req)
                       (sleep 1)
                       (box-update! total add1)
                       (response/output void)))))

       #:after
       (lambda ()
         (stop))

       (test-case "drops events when the queue is full"
         (parameterize ([current-sentry (make-sentry
                                         #:backlog 1
                                         "http://test@127.0.0.1:9095/test")])
           (for ([_ (in-range 100)])
             (sentry-capture-exception! e))
           (sentry-stop)
           (check-equal? (unbox total) 2))))))))

(module+ test
  (require rackunit/text-ui)
  (run-tests sentry-tests))
