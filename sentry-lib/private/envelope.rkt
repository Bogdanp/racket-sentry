#lang racket/base

(require file/sha1
         json
         racket/random
         "date.rkt"
         "event.rkt"
         "hasheq-sugar.rkt")

(provide
 envelope-payload)

(define ((envelope-payload data) hs)
  (define event-bs (jsexpr->bytes (event->jsexpr data)))
  (define out (open-output-bytes))
  (write-json
   {event_id (generate-event-id)
    sent_at (date->rfc3339 (current-utc-date))}
   out)
  (write-char #\newline out)
  (write-json
   {type "event"
    length (bytes-length event-bs)}
   out)
  (write-char #\newline out)
  (write-bytes event-bs out)
  (write-char #\newline out)
  (values
   (hash-set hs 'content-type #"application/x-sentry-envelope")
   (get-output-bytes out)))

(define (generate-event-id)
  (bytes->hex-string (crypto-random-bytes 16)))

;; Local variables:
;; racket-indent-sequence-depth: 1
;; End:
