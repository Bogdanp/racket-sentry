#lang info

(define license 'BSD-3-Clause)
(define collection "sentry")
(define scribblings '(("scribblings/sentry.scrbl" () ("Web Development"))))

(define deps '("base"))
(define build-deps '("gregor-lib"
                     "sentry-lib"
                     "scribble-lib"
                     "web-server-lib"

                     "gregor-doc"
                     "racket-doc"
                     "web-server-doc"))
(define update-implies '("sentry-lib"))
