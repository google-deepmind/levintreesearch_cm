#lang info
(define collection "lts-cm")
(define deps '("base"
               "data-lib"
               "define2"
               "draw-lib"
               "global"
               "gui-lib"
               "jobsched"
               "math-lib"
               "pict-lib"
               "plot-gui-lib"
               "plot-lib"
               "rackunit-lib"
               "safe-case"
               "text-block"
               "text-table"
               "timev"))
(define build-deps '("scribble-lib" "racket-doc" "rackunit-lib"))
(define scribblings '(("scribblings/lts-cm.scrbl" ())))
(define pkg-desc "LevinTreeSearch with Context Models — main collection")
(define version "0.0")
(define pkg-authors '(lorseau))
(define license 'Apache-2.0)
(define test-omit-paths '("domains/sokoban/example-optim-gui.rkt"))
