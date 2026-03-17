#lang info
(define collection "levintreesearch_cm")
(define deps '("base"
               "lts-cm"
               "jobsched"
               "timev"))
(define implies '("lts-cm" "jobsched" "timev"))
(define build-deps '())
(define pkg-desc "LevinTreeSearch with Context Models")
(define version "0.0")
(define pkg-authors '(lorseau))
(define license 'Apache-2.0)
