#lang racket/base

(require json
         net/http-client
         net/url
         racket/async-channel
         racket/contract
         racket/format
         racket/function
         racket/list
         racket/match
         racket/port
         racket/string
         "private/event.rkt"
         "private/http.rkt")

(provide
 current-sentry
 make-sentry
 sentry?
 sentry-capture-exception!)

(define-logger sentry)

(struct sentry (dsn release environment custodian chan dispatcher)
  #:transparent)

(define/contract current-sentry
  (parameter/c (or/c false/c sentry?))
  (make-parameter #f))

(define/contract (make-sentry dsn:str
                              #:backlog [backlog 128]
                              #:release [release (getenv "SENTRY_RELEASE")]
                              #:environment [environment (getenv "SENTRY_ENVIRONMENT")])
  (->* (string?)
       (#:backlog exact-positive-integer?
        #:release (or/c false/c non-empty-string?)
        #:environment (or/c false/c non-empty-string?))
       sentry?)
  (define dsn (string->url dsn:str))
  (define auth (dsn->auth dsn))
  (define endpoint (dsn->endpoint dsn))
  (define custodian (make-custodian))
  (parameterize ([current-custodian custodian])
    (define chan (make-async-channel backlog))
    (define dispatcher (make-sentry-dispatcher chan auth endpoint))
    (sentry dsn release environment custodian chan dispatcher)))

(define (dsn->auth dsn)
  (with-output-to-bytes
    (lambda _
      (define-values (key secret)
        (match (string-split (or (url-user dsn) "") ":")
          [(list key)        (values key #f)]
          [(list key secret) (values key secret)]))

      (display "Sentry sentry_version=7, sentry_client=racket-sentry/0.0.1, ")
      (display (format "sentry_timestamp=~a, " (current-seconds)))
      (display (format "sentry_key=~a" key))
      (when secret
        (display (format ", sentry_secret=~a" secret))))))

(define (dsn->endpoint dsn)
  (define-values (path project-id)
    (split-at-right (url-path dsn) 1))

  (~a (url-scheme dsn)
      "://"
      (url-host dsn)
      ":"
      (url-port/safe dsn)
      (url-path->string path)
      "/api/"
      (url-path->string project-id)
      "/store/"))

(define (make-sentry-dispatcher chan auth endpoint)
  (thread
   (lambda _
     (define endpoint:url (string->url endpoint))
     (define conn (http-conn-open (url-host endpoint:url)
                                  #:port (url-port/safe endpoint:url)
                                  #:ssl? (url-ssl? endpoint:url)
                                  #:auto-reconnect? #t))

     (define rate-limit-deadline (current-seconds))
     (define (rate-limited?)
       (< (current-seconds) rate-limit-deadline))

     (define (send-event! e)
       (http-conn-sendrecv! conn endpoint
                            #:method "POST"
                            #:headers (list "Content-type: application/json; charset=utf-8"
                                            "User-Agent: racket-sentry/0.0.1"
                                            (~a "X-Sentry-Auth: " auth))
                            #:data (call-with-output-bytes
                                    (curry write-json (event->jsexpr e)))))

     (let loop ()
       (with-handlers ([exn:fail?
                        (lambda (e)
                          (log-sentry-error "failed to dispatch: ~a" (exn-message e))
                          (loop))])
         (define e (sync chan))
         (cond
           [(rate-limited?)
            (log-sentry-warning "dropping event ~v due to rate limit" e)
            (loop)]

           [else
            (log-sentry-debug "capturing event ~v" e)
            (define-values (status headers response)
              (send-event! e))

            (log-sentry-debug "received response ~v" status)
            (define response:bytes (port->bytes response))
            (match (status-line->code status)
              [429
               (log-sentry-warning "rate limit reached")
               (define retry-after
                 (string->number
                  (hash-ref (headers->hash headers) "retry-after" "")))

               (when retry-after
                 (log-sentry-warning "dropping all events for the next ~a seconds" retry-after)
                 (set! rate-limit-deadline (+ (current-seconds) retry-after)))]

              [200
               (log-sentry-debug "event captured successfully")]

              [_
               (log-sentry-warning UNEXPECTED-RESPONSE-MESSAGE
                                   status headers response:bytes)])

            (loop)]))))))

(define sentry-capture-exception!
  (make-keyword-procedure
   (lambda (kws kw-args e [s (current-sentry)] . args)
     (when s
       (define evt
         (let ([evt (keyword-apply make-event kws kw-args (cons e args))])
           (struct-copy event evt
                        [environment (or (event-environment evt) (sentry-environment s))]
                        [release (or (event-release evt) (sentry-release s))])))
       (async-channel-put (sentry-chan s) evt)))))

(define UNEXPECTED-RESPONSE-MESSAGE #<<MESSAGE
unexpected response from Sentry server

status: ~a
headers: ~v
data: ~a
MESSAGE
  )
