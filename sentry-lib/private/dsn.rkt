#lang racket/base

(require net/url
         racket/format
         racket/list
         racket/match
         racket/port
         racket/string
         "reflect.rkt")

(provide
 dsn->auth
 dsn->endpoint)

(define (dsn->auth dsn)
  (with-output-to-bytes
    (lambda ()
      (define-values (key secret)
        (match (string-split (or (url-user dsn) "") ":")
          [(list key)        (values key #f)]
          [(list key secret) (values key secret)]))

      (printf "Sentry sentry_version=7, sentry_client=~a, " lib-version)
      (printf "sentry_timestamp=~a, " (current-seconds))
      (printf "sentry_key=~a" key)
      (when secret
        (printf ", sentry_secret=~a" secret)))))

(define (dsn->endpoint dsn)
  (define-values (path project-id)
    (split-at-right (url-path dsn) 1))

  (~a (url-scheme dsn)
      "://"
      (url-host dsn)
      ":"
      (url-port* dsn)
      (url-path->string path)
      "/api/"
      (url-path->string project-id)
      "/store/"))

(define (url-path->string p)
  (string-join (map path/param-path p) "/"))

(define (url-port* u)
  (or (url-port u)
      (case (url-scheme u)
        [("https") 443]
        [else      80])))
