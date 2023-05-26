#lang info
(define collection "lts-cm")
(define deps '("base"
               "define2"
               "global"
               "gui-lib"
               "jobsched"
               "safe-case"
               "timev"
               "text-table"
               "text-block"))
(define build-deps '("scribble-lib" "racket-doc" "rackunit-lib"))
(define scribblings '(("scribblings/lts-cm.scrbl" ())))
(define pkg-desc "LevinTreeSearch with Context Models — main collection")
(define version "0.0")
(define pkg-authors '(lorseau))
(define license 'Apache-2.0)
(define test-omit-paths '("domains/sokoban/example-optim-gui.rkt"))
