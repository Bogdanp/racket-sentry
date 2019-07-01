#lang racket/base

(require gregor
         racket/contract
         racket/date
         racket/format
         racket/function
         racket/hash
         racket/match
         racket/string
         "user.rkt")

(provide
 make-event
 event?
 event->jsexpr)

(struct event (e level timestamp transaction server-name environment release tags user)
  #:transparent)

(define/contract (make-event e
                             #:level [level 'error]
                             #:timestamp [timestamp (now/moment)]
                             #:transaction [transaction #f]
                             #:server-name [server-name #f]
                             #:environment [environment #f]
                             #:release [release #f]
                             #:tags [tags (hash)])
  (->* (exn?)
       (#:level (or/c 'fatal 'error 'warning 'info 'debug)
        #:timestamp moment?
        #:transaction (or/c false/c non-empty-string?)
        #:server-name (or/c false/c non-empty-string?)
        #:environment (or/c false/c non-empty-string?)
        #:release (or/c false/c non-empty-string?)
        #:tags (hash/c non-empty-string? string?))
       event?)
  (event e
         level
         timestamp
         transaction
         server-name
         environment
         release
         tags
         (current-sentry-user)))

(define OPTIONAL-EVENT-ACCESSORS
  (hasheq 'transaction event-transaction
          'server_name event-server-name
          'environment event-environment
          'release event-release
          'tags event-tags
          'user event-user))

(define (event->jsexpr e)
  (for/fold ([data (hasheq 'platform "other"
                           'level (symbol->string (event-level e))
                           'timestamp (moment->iso8601 (event-timestamp e))
                           'exception (exn->exception (event-e e)))])
            ([(key accessor) (in-hash OPTIONAL-EVENT-ACCESSORS)])
    (cond
      [(accessor e) => (curry hash-set data key)]
      [else data])))

(define (exn->exception e)
  (define ctx (continuation-mark-set->context (exn-continuation-marks e)))
  (hasheq 'values (list (hasheq 'type (symbol->string (object-name e))
                                'value (exn-message e)
                                'stacktrace (hasheq 'frames (ctx->frames ctx))))))

(define (ctx->frames ctx)
  (for/list ([frame (in-list ctx)])
    (match-define (cons proc loc) frame)
    (hash-union
     (hash 'function (symbol->string (or proc 'unknown)))
     (if loc
         (hasheq 'abs_path (if (path? (srcloc-source loc))
                               (path->string (srcloc-source loc))
                               (~a (srcloc-source loc)))
                 'lineno (or (srcloc-line loc) 0))
         (hasheq)))))
