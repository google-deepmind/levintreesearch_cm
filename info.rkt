#lang info
(define collection 'multi)
(define deps '("data-lib"
               "define2"
               "global"
               "gui-lib"
               "math-lib"
               "pict-lib"
               "plot-gui-lib"
               "plot-lib"
               "rackunit-lib"
               "safe-case"
               "text-table"
               "text-block"
               "base"))
(define build-deps '("distributed-places-doc"
                     "scribble-lib" "racket-doc" "rackunit-lib"))
(define pkg-desc "LevinTreeSearch with Context Models")
(define version "0.0")
(define pkg-authors '(lorseau))
(define license 'Apache-2.0)
