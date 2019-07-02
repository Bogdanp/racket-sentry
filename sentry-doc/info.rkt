#lang info

(define collection "sentry")
(define scribblings '(("scribblings/sentry.scrbl" ())))

(define deps '("base"))
(define build-deps '("gregor-lib"
                     "sentry-lib"
                     "scribble-lib"
                     "web-server-lib"

                     "gregor-doc"
                     "racket-doc"
                     "web-server-doc"))
(define update-implies '("sentry-lib"))
