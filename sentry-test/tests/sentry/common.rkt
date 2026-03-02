#lang racket/base

(require racket/async-channel
         web-server/servlet-dispatch
         web-server/web-server)

(provide
 make-server
 test-dsn)

(define (make-server start)
  (define ch (make-async-channel))
  (begin0 (serve
           #:confirmation-channel ch
           #:port 9095
           #:dispatch (dispatch/servlet start))
    (sync ch)))

(define test-dsn
  (getenv "SENTRY_TEST_DSN"))
