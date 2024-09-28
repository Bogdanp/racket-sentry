#lang racket/base

(require racket/lazy-require
         racket/match)

(lazy-require
 [gregor (moment->iso8601)])

(provide
 current-utc-date
 date->rfc3339
 ->rfc3339)

(define (->rfc3339 v)
  (if (date*? v)
      (date->rfc3339 v)
      (moment->iso8601 v)))

(define (current-utc-date)
  (seconds->date (* (current-inexact-milliseconds) 0.001) #f))

(define (date->rfc3339 the-date)
  (match-define (date* second minute hour day month year _week-day _year-day _dst? 0 nanosecond "UTC")
    the-date)
  (define out (open-output-string))
  (write-string (pad year 4) out)
  (write-char #\- out)
  (write-string (pad month 2) out)
  (write-char #\- out)
  (write-string (pad day 2) out)
  (write-char #\T out)
  (write-string (pad hour 2) out)
  (write-char #\: out)
  (write-string (pad minute 2) out)
  (write-char #\: out)
  (write-string (pad second 2) out)
  (write-char #\. out)
  (write-string (pad nanosecond 9) out)
  (write-char #\Z out)
  (get-output-string out))

(define (pad n len)
  (define dst (make-string len #\0))
  (define src (number->string n))
  (string-copy! dst (- len (string-length src)) src)
  dst)
