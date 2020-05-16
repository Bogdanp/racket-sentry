#lang racket/base

(require net/url
         racket/string)

(provide
 url-path->string
 url-port/safe
 url-ssl?

 headers-ref)

(define (url-path->string p)
  (string-join (map path/param-path p) "/"))

(define (url-ssl? u)
  (or (equal? (url-scheme u) "https")
      (equal? (url-port u) 443)))

(define (url-port/safe u)
  (or (url-port u)
      (case (url-scheme u)
        [("https") 443]
        [else      80])))

(define (headers-ref hs name)
  (define rx (byte-regexp (bytes-append #"^(?i:" name #":)")))
  (for/first ([h (in-list hs)]
              #:when (regexp-match? rx h))
    (subbytes h (+ 2 (bytes-length name)))))
