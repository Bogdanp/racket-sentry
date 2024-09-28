#lang racket/base

(require racket/os
         racket/promise
         "hasheq-sugar.rkt")

(provide
 get-common-contexts)

(define (get-common-contexts)
  (force contexts))

(define contexts
  (delay/sync
   {device {name (gethostname)}
    os {raw_description (system-type 'machine)}
    runtime {name (case (system-type 'vm)
                    [(racket) "Racket BC"]
                    [(chez-scheme) "Racket CS"]
                    [else (format "Racket ~a" (system-type 'vm))])
             version (version)}}))

;; Local variables:
;; racket-indent-sequence-depth: 1
;; End:
