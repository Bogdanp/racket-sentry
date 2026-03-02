#lang racket/base

(require file/gunzip
         json
         rackunit
         sentry
         sentry/cron
         sentry/tracing
         web-server/http
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
        (sentry-stop c)))

    (let ([stop #f]
          [reqs null])
      (define (get-reqs)
        (begin0 (reverse reqs)
          (set! reqs null)))

      (test-suite
       "stub server"
       #:before
       (lambda ()
         (define stop-proc
           (make-server
            (lambda (req)
              (set! reqs (cons req reqs))
              (response/empty))))
         (set! stop stop-proc))
       #:after
       (lambda ()
         (stop))

       (test-case "sends the right sequence of events"
         (parameterize ([current-sentry (make-sentry "http://test@127.0.0.1:9095/test")])
           (call-with-transaction
             "example-transaction"
             (lambda (_t)
               (call-with-monitor
                "example-cron-job"
                #:config
                (monitor-config
                 (schedule 'crontab "* * * * *"))
                (lambda ()
                  (sleep 1)))))
           (sentry-stop (current-sentry)))
         (define triples
           (for/list ([req (in-list (get-reqs))])
             (define-values (in out)
               (make-pipe))
             (thread
              (lambda ()
                (gunzip-through-ports
                 (open-input-bytes
                  (request-post-data/raw req))
                 out)
                (close-output-port out)))
             (for/list ([data (in-producer read-json eof in)])
               data)) )
         (check-match
          triples
          (list
           (list
            _
            (hash* ['type "check_in"])
            (hash* ['check_in_id id]
                   ['monitor_slug slug]
                   ['status "in_progress"]))
           (list
            _
            (hash* ['type "check_in"])
            (hash* ['check_in_id id]
                   ['monitor_slug slug]
                   ['status "ok"]))
           (list
            _
            (hash* ['type "transaction"])
            (hash* ['transaction "example-transaction"]))))))))))

(module+ test
  (require rackunit/text-ui)
  (run-tests cron-tests))
