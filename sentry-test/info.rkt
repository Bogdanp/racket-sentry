#lang info

(define license 'BSD-3-Clause)
(define collection 'multi)

(define deps '())
(define build-deps '("base"
                     "gregor-lib"
                     "rackunit-lib"
                     "sentry-lib"
                     "threading-lib"
                     "web-server-lib"))

(define update-implies '("sentry-lib"))
