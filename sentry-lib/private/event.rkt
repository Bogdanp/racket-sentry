#lang racket/base

(require json
         net/url
         racket/contract/base
         racket/format
         racket/os
         racket/string
         web-server/http/request-structs
         "date.rkt"
         "hasheq-sugar.rkt"
         "user.rkt")


;; breadcrumb ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide
 (contract-out
  [make-breadcrumb
   (->* [#:message string?]
        [#:timestamp any/c
         #:category (or/c 'log)
         #:level (or/c #f 'debug 'info 'warning 'error 'fatal)
         #:data (or/c #f jsexpr?)]
        breadcrumb?)]))

(struct breadcrumb (timestamp category level message data)
  #:transparent)

(define (make-breadcrumb
         #:message message
         #:timestamp [timestamp (current-utc-date)]
         #:category [category 'log]
         #:level [level 'warning]
         #:data [data #f])
  (breadcrumb timestamp category level message data))


;; event ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide
 (contract-out
  [struct event
    ([e exn?]
     [level level/c]
     [timestamp any/c] #;(or/c date* moment?)
     [transaction maybe-non-empty-string/c]
     [server-name maybe-non-empty-string/c]
     [environment maybe-non-empty-string/c]
     [release maybe-non-empty-string/c]
     [request (or/c #f request?)]
     [tags (hash/c non-empty-string? string?)]
     [user (or/c #f sentry-user?)]
     [breadcrumbs (listof breadcrumb?)])]
  [make-event
   (->* [exn?]
        [#:level (or/c 'fatal 'error 'warning 'info 'debug)
         #:timestamp any/c
         #:transaction (or/c #f non-empty-string?)
         #:server-name (or/c #f non-empty-string?)
         #:environment (or/c #f non-empty-string?)
         #:release (or/c #f non-empty-string?)
         #:request (or/c #f request?)
         #:tags (hash/c non-empty-string? string?)
         #:user (or/c #f sentry-user?)
         #:breadcrumbs (listof breadcrumb?)]
        event?)]
  [event-attach-breadcrumbs
   (-> event? (listof breadcrumb?) event?)]
  [event->jsexpr
   (-> event? jsexpr?)]))

(define level/c
  (or/c 'fatal 'error 'warning 'info 'debug))

(define maybe-non-empty-string/c
  (or/c #f non-empty-string?))

(struct event
  (e
   level
   timestamp
   transaction
   server-name
   environment
   release
   request
   tags
   user
   breadcrumbs)
  #:transparent)

(define (make-event
         e
         #:level [level 'error]
         #:timestamp [timestamp (current-utc-date)]
         #:transaction [transaction #f]
         #:server-name [server-name #f]
         #:environment [environment #f]
         #:release [release #f]
         #:request [request #f]
         #:tags [tags (hash)]
         #:user [user (current-sentry-user)]
         #:breadcrumbs [breadcrumbs null])
  (event e
         level
         timestamp
         transaction
         server-name
         environment
         release
         request
         tags
         user
         breadcrumbs))

(define (event-attach-breadcrumbs e crumbs)
  (struct-copy event e [breadcrumbs crumbs]))

(define (event->jsexpr e)
  (for*/hasheq ([(key accessor) (in-hash accessors)]
                [value (in-value (accessor e))]
                #:when value)
    (values key value)))

(define (exn->jsexpr e)
  (define ctx (continuation-mark-set->context (exn-continuation-marks e)))
  {values (list {type (symbol->string (object-name e))
                 value (exn-message e)
                 stacktrace {frames (ctx->jsexpr ctx)}})})

(define (ctx->jsexpr ctx)
  (for*/list ([frame (in-list (reverse ctx))]
              [proc (in-value (car frame))]
              [loc (in-value (cdr frame))]
              [fun (in-value (~a (or proc 'unknown)))])
    (if loc
        {function fun
         abs_path (if (path? (srcloc-source loc))
                      (path->string (srcloc-source loc))
                      (~a (srcloc-source loc)))
         lineno (or (srcloc-line loc) 0)}
        {function fun})))

(define (breadcrumb->jsexpr crumb)
  {timestamp (->rfc3339 (breadcrumb-timestamp crumb))
   category (symbol->string (breadcrumb-category crumb))
   message (breadcrumb-message crumb)
   level (symbol->string (breadcrumb-level crumb))
   data (or (breadcrumb-data crumb) (hasheq))})

(define (request->jsexpr req)
  {url (url->string (request-uri req))
   method (bytes->string/utf-8 (request-method req))
   headers (for/hasheq ([hdr (in-list (request-headers/raw req))])
             (values (string->symbol (bytes->string/utf-8 (header-field hdr)))
                     (bytes->string/utf-8 (header-value hdr))))})

(define contexts
  {device {name (gethostname)}
   os {raw_description (system-type 'machine)}
   runtime {name (case (system-type 'vm)
                   [(racket) "Racket BC"]
                   [(chez-scheme) "Racket CS"]
                   [else (format "Racket ~a" (system-type 'vm))])
            version (version)}})

(define accessors
  {platform (λ (_) "other")
   level (compose1 symbol->string event-level)
   timestamp (compose1 ->rfc3339 event-timestamp)
   exception (compose1 exn->jsexpr event-e)
   transaction event-transaction
   server_name event-server-name
   environment event-environment
   release event-release
   request (lambda (e)
             (define request (event-request e))
             (and request (request->jsexpr request)))
   tags event-tags
   user (lambda (e)
          (define user (event-user e))
          (and user (sentry-user->jsexpr user)))
   contexts (λ (_) contexts)
   breadcrumbs (lambda (e)
                 (define crumbs (event-breadcrumbs e))
                 (and (not (null? crumbs))
                      (map breadcrumb->jsexpr crumbs)))})


;; Local variables:
;; racket-indent-sequence-depth: 1
;; End:
