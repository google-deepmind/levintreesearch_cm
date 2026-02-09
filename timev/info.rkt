#lang info

(define collection "timev")
(define deps '("base" "define2"))
(define build-deps '("scribble-lib" "racket-doc" "rackunit-lib"))
(define scribblings '(("scribblings/timev.scrbl" ())))
(define pkg-desc "Similar to `time` with more options")
(define version "0.0")
(define pkg-authors '(lorseau))
(define license 'Apache-2.0)
