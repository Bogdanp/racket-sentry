#lang racket/base

(require (only-in db query-exec sqlite3-connect)
         racket/async-channel
         rackunit
         sentry
         sentry/tracing
         sentry/tracing/database
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
        (sync (system-idle-evt))
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
          [total (box 0)]
          [sema (make-semaphore)])
      (test-suite
       "retries"

       #:before
       (lambda ()
         (set! stop (make-server
                     (lambda (_req)
                       (box-update! total add1)
                       (define thd
                         (current-thread))
                       (thread
                        (lambda ()
                          (sync (thread-dead-evt thd))
                          (semaphore-post sema)))
                       (response/output
                        #:code 429
                        #:message #"Too Many Requests"
                        #:headers (list (make-header #"Retry-After" #"1"))
                        void)))))

       #:after
       (lambda ()
         (stop))

       (test-case "respects rate limits"
         (parameterize ([current-sentry (make-sentry "http://test@127.0.0.1:9095/test")])
           (sentry-capture-exception! e)
           (semaphore-wait sema)
           (for ([_ (in-range 10)])
             (sentry-capture-exception! e))
           (sleep 1.1)
           (sync (system-idle-evt))
           (sentry-capture-exception! e)
           (sync (system-idle-evt))
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
           (check-equal? (unbox total) 1))))))

   (test-suite
    "call-with-transaction"

    (when test-dsn
      (test-case "nesting"
        (define c (make-sentry test-dsn))
        (parameterize ([current-sentry c])
          (call-with-transaction "GET /"
            #:operation 'http.server
            #:source 'url
            #:data (hasheq
                    'http.request.method "GET"
                    'url.path "/"
                    'url.scheme "http")
            (lambda (t)
              (define conn
                (trace-connection
                 (sqlite3-connect #:database 'memory)))
              (query-exec conn "SELECT $1" 42)
              (call-with-span #:operation 'view.render void)
              (span-set! t 'http.response.status_code 200))))
        (sentry-stop c))

      (test-case "error in txn"
        (define c (make-sentry test-dsn))
        (parameterize ([current-sentry c])
          (call-with-transaction "GET /"
            #:operation 'http.server
            #:source 'url
            (lambda (_)
              (call-with-span
               #:description "do-something"
               (lambda (_)
                 (sentry-capture-exception! (exn:fail "an error in txn" (current-continuation-marks))))))))
        (sentry-stop c))))

   (let ([stop #f]
         [events null])
     (test-suite
      "sampling"

      #:before
      (lambda ()
        (set! stop (make-server
                    (lambda (req)
                      (set! events (cons (request-post-data/raw req) events))
                      (response/empty)))))

      #:after
      (lambda ()
        (stop))

      (test-case "can sample events"
        (define (sample-rate e)
          (cond
            [(and (transaction? e)
                  (equal? (transaction-name e) "GET /_health"))
             0.0]
            [else 1.0]))

        (parameterize ([current-sentry
                        (make-sentry
                         #:sampler sample-rate
                         "http://test@127.0.0.1:9095/test")])
          (call-with-transaction "GET /"
            #:operation 'http.server
            #:source 'url
            void)
          (call-with-transaction "GET /_health"
            #:operation 'http.server
            #:source 'url
            void)
          (sentry-stop)
          (check-equal? (length events) 1)))))))

(module+ test
  (require rackunit/text-ui)
  (run-tests sentry-tests))
