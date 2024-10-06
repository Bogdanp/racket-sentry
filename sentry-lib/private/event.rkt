#lang racket/base

(require net/url
         racket/format
         racket/hash
         web-server/http/request-structs
         "context.rkt"
         "date.rkt"
         "hasheq-sugar.rkt"
         "user.rkt")


;; breadcrumb ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide
 make-breadcrumb
 breadcrumb?)

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
 (struct-out event)
 make-event
 event-attach-breadcrumbs
 event->jsexpr)

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
   breadcrumbs
   trace-context)
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
         #:breadcrumbs [breadcrumbs null]
         #:trace-context [trace-context #f])
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
         breadcrumbs
         trace-context))

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
    (define path
      (and loc (if (path? (srcloc-source loc))
                   (path->string (srcloc-source loc))
                   (~a (srcloc-source loc)))))
    (if loc
        {function fun
         abs_path path
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
   contexts (λ (e)
              (cond
                [(event-trace-context e)
                 => (lambda (trace-context)
                      (hash-union
                       (get-common-contexts)
                       {trace trace-context}))]
                [else
                 (get-common-contexts)]))
   breadcrumbs (lambda (e)
                 (define crumbs (event-breadcrumbs e))
                 (and (not (null? crumbs))
                      (map breadcrumb->jsexpr crumbs)))})

;; Local variables:
;; racket-indent-sequence-depth: 1
;; End:
