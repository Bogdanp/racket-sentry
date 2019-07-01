#lang racket/base

(provide test-dsn)

(define test-dsn
  (getenv "SENTRY_TEST_DSN"))
