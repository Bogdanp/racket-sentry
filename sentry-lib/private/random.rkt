#lang racket/base

(require file/sha1
         racket/random)

(provide
 generate-random-id)

(define (generate-random-id [n 16])
  (bytes->hex-string (crypto-random-bytes n)))
