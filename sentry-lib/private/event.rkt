#lang racket/base

(require gregor
         json
         mzlib/os
         net/url
         racket/contract/base
         racket/format
         racket/string
         web-server/http/request-structs
         "user.rkt")


;; breadcrumb ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide
 (contract-out
  [make-breadcrumb
   (->* [#:message string?]
        [#:timestamp moment?
         #:category (or/c 'log)
         #:level (or/c #f 'debug 'info 'warning 'error 'fatal)
         #:data (or/c #f jsexpr?)]
        breadcrumb?)]))

(struct breadcrumb (timestamp category level message data)
  #:transparent)

(define (make-breadcrumb #:message message
                         #:timestamp [timestamp (now/moment)]
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
     [timestamp moment?]
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
         #:timestamp moment?
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

(define (make-event e
                    #:level [level 'error]
                    #:timestamp [timestamp (now/moment)]
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
  (hasheq 'values (list (hasheq 'type (symbol->string (object-name e))
                                'value (exn-message e)
                                'stacktrace (hasheq 'frames (ctx->jsexpr ctx))))))

(define (ctx->jsexpr ctx)
  (for*/list ([frame (in-list (reverse ctx))]
              [proc (in-value (car frame))]
              [loc (in-value (cdr frame))]
              [fun (in-value (~a (or proc 'unknown)))])
    (if loc
        (hasheq 'function fun
                'abs_path (if (path? (srcloc-source loc))
                              (path->string (srcloc-source loc))
                              (~a (srcloc-source loc)))
                'lineno (or (srcloc-line loc) 0))
        (hasheq 'function fun))))

(define (breadcrumb->jsexpr crumb)
  (hasheq
   'timestamp (moment->iso8601 (breadcrumb-timestamp crumb))
   'category (symbol->string (breadcrumb-category crumb))
   'message (breadcrumb-message crumb)
   'level (symbol->string (breadcrumb-level crumb))
   'data (or (breadcrumb-data crumb) (hasheq))))

(define (request->jsexpr req)
  (hasheq
   'url (url->string (request-uri req))
   'method (bytes->string/utf-8 (request-method req))
   'headers (for/hasheq ([hdr (in-list (request-headers/raw req))])
              (values (string->symbol (bytes->string/utf-8 (header-field hdr)))
                      (bytes->string/utf-8 (header-value hdr))))))

(define contexts
  (hasheq
   'device (hasheq
            'name (gethostname))
   'os (hasheq
        'raw_description (system-type 'machine))
   'runtime (hasheq
             'name (case (system-type 'vm)
                     [(racket) "Racket BC"]
                     [(chez-scheme) "Racket CS"]
                     [else (format "Racket ~a" (system-type 'vm))])
             'version (version))))

(define accessors
  (hasheq
   'platform (lambda (_) "other")
   'level (compose1 symbol->string event-level)
   'timestamp (compose1 moment->iso8601 event-timestamp)
   'exception (compose1 exn->jsexpr event-e)
   'transaction event-transaction
   'server_name event-server-name
   'environment event-environment
   'release event-release
   'request (lambda (e)
              (define request (event-request e))
              (and request (request->jsexpr request)))
   'tags event-tags
   'user (lambda (e)
           (define user (event-user e))
           (and user (sentry-user->jsexpr user)))
   'contexts (lambda (_)
               contexts)
   'breadcrumbs (lambda (e)
                  (define crumbs (event-breadcrumbs e))
                  (and (not (zero? (length crumbs)))
                       (hasheq 'values (map breadcrumb->jsexpr (event-breadcrumbs e)))))))
