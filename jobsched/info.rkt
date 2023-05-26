#lang info
(define collection "jobsched")
(define deps '("data-lib"
               "define2"
               "global"
               "base"
               "timev"))
(define build-deps '("scribble-lib" "racket-doc" "rackunit-lib"))
(define scribblings '(("scribblings/jobsched.scrbl" ())))
(define pkg-desc "Job scheduler — spawns multiple Racket instances")
(define version "0.1")
(define pkg-authors '(lorseau))
(define license 'Apache-2.0)
(define test-omit-paths '("examples"))
