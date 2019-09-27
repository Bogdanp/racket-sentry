#lang racket/base

(require net/url
         racket/match
         racket/string)

(provide
 url-path->string
 url-port/safe
 url-ssl?

 status-line->code
 headers->hash)

(define (url-path->string p)
  (string-join (map path/param-path p) "/"))

(define (url-ssl? u)
  (or (equal? (url-scheme u) "https")
      (equal? (url-port u) 443)))

(define (url-port/safe u)
  (or (url-port u)
      (case (url-scheme u)
        [("http")  80]
        [("https") 443]
        [else 80])))

(define status-code-re
  #rx"^HTTP.... ([^ ]+) ")

(define (status-line->code s)
  (match-define (list _ code)
    (regexp-match status-code-re s))

  (string->number
   (bytes->string/utf-8 code)))

(define header-re
  #rx"^([^:]+): (.*)$")

(define (headers->hash hs)
  (for/fold ([res (hash)])
            ([h (in-list hs)])
    (match-define (list _ name value)
      (regexp-match header-re h))

    (hash-set res
              (string-downcase (bytes->string/utf-8 name))
              (bytes->string/utf-8 value))))
