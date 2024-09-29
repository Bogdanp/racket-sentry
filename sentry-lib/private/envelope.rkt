#lang racket/base

(require json
         racket/match
         "date.rkt"
         "event.rkt"
         "hasheq-sugar.rkt"
         "random.rkt"
         "transaction.rkt")

(provide
 envelope-payload)

(define-logger sentry)

(define ((envelope-payload e) hs)
  (define out (open-output-bytes))
  (define-values (type id data)
    (match e
      [(? event? e)
       (values
        "event"
        (generate-random-id)
        (jsexpr->bytes
         (event->jsexpr e)))]
      [(? transaction? t)
       (values
        "transaction"
        (generate-random-id)
        (jsexpr->bytes
         (transaction->jsexpr t)))]))
  (write-json
   {event_id id
    sent_at (date->rfc3339 (current-utc-date))}
   out)
  (write-char #\newline out)
  (write-json
   {type type
    length (bytes-length data)}
   out)
  (write-char #\newline out)
  (write-bytes data out)
  (write-char #\newline out)
  (define envelope-bs
    (get-output-bytes out))
  (log-sentry-debug "sending envelope: ~s" envelope-bs)
  (values (hash-set hs 'content-type #"application/x-sentry-envelope") envelope-bs))

;; Local variables:
;; racket-indent-sequence-depth: 1
;; End:
