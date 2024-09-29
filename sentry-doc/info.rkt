#lang info

(define license 'BSD-3-Clause)
(define collection "sentry")
(define scribblings
  '(("scribblings/sentry.scrbl" () ("Web Development"))))
(define deps '("base"))
(define build-deps
  '("db-doc"
    "db-lib"
    "gregor-doc"
    "gregor-lib"
    "racket-doc"
    "scribble-lib"
    "sentry-lib"
    "web-server-doc"
    "web-server-lib"))
(define update-implies
  '("sentry-lib"))
