#lang info

(define collection 'multi)

(define deps '())
(define build-deps '("base"
                     "gregor-lib"
                     "rackunit-lib"
                     "sentry-lib"
                     "web-server-lib"))

(define update-implies '("sentry-lib"))
