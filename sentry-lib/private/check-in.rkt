#lang racket/base

(require buid
         racket/match
         threading
         "hasheq-sugar.rkt")

(provide
 (struct-out check-in)
 (struct-out monitor-config)
 (struct-out schedule)
 make-check-in
 check-in->jsexpr)

(struct check-in (id monitor-slug status duration environment release monitor-config contexts)
  #:transparent)
(struct monitor-config (schedule)
  #:transparent)
(struct schedule (type value)
  #:transparent)

(define (make-check-in
         #:id [id (buid->uuid (buid))]
         #:status status
         #:duration [duration #f]
         #:environment [environment #f]
         #:release [release #f]
         #:contexts [contexts #f]
         slug [config #f])
  (check-in
   #;id id
   #;monitor-slug slug
   #;status status
   #;duration duration
   #;environment environment
   #;release release
   #;monitor-config config
   #;contexts contexts))

(define (~check-in-status status)
  (case status
    [(in-progress) "in_progress"]
    [else (symbol->string status)]))

(define (check-in->jsexpr ci)
  (for*/hasheq ([(key accessor) (in-hash accessors)]
                [value (in-value (accessor ci))]
                #:when value)
    (values key value)))

(define (monitor-config->jsexpr c)
  (match-define (monitor-config s) c)
  (hasheq 'schedule (schedule->jsexpr s)))

(define (schedule->jsexpr s)
  (match-define (schedule t v) s)
  {type (symbol->string t)
   value v})

(define accessors
  {check_in_id check-in-id
   environment check-in-environment
   release check-in-release
   status (compose1 ~check-in-status check-in-status)
   duration check-in-duration
   monitor_slug check-in-monitor-slug
   monitor_config (lambda-and~> check-in-monitor-config monitor-config->jsexpr)
   contexts check-in-contexts})

;; Local variables:
;; racket-indent-sequence-depth: 1
;; End:
