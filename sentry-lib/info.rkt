#lang info

(define license 'BSD-3-Clause)
(define version "0.6")
(define collection "sentry")
(define deps
  '("actor-lib"
    "base"
    ["box-extra-lib" #:version "1.0.1"]
    "http-easy-lib"
    "monocle-lib"
    "threading-lib"
    "web-server-lib"))
(define build-deps '())
