#lang racket/base
#| Copyright 2023 DeepMind Technologies Limited.

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

https://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.|#

(require racket/math
         lts-cm/byte-board
         pict
         "sokoban.rkt"
         define2)

(provide sokoban->pict)

(define cell-size 32)

(define empty-cell-pict
  (filled-rectangle cell-size cell-size #:color "white" #:border-color "black" #:border-width 1))
(define wall-pict
  (filled-rectangle cell-size cell-size #:color "gray" #:border-color "black" #:border-width 1))
(define goal-pict
  (cc-superimpose
   empty-cell-pict
   (rounded-rectangle (exact-floor (/ cell-size (sqrt 2))) (exact-floor (/ cell-size (sqrt 2)))
                      #:angle (/ pi 4) #:border-color "blue" #:border-width 3)))
(define box-alone-pict
  (filled-rectangle (* 3/4 cell-size) (* 3/4 cell-size) #:color "brown" #:border-color "black" #:border-width 1))
(define box-pict         (cc-superimpose empty-cell-pict box-alone-pict))
(define player-alone-pict (jack-o-lantern (exact-floor (/ cell-size 1.1))))
(define player-pict      (cc-superimpose empty-cell-pict player-alone-pict))
(define goal+box-pict    (cc-superimpose goal-pict box-alone-pict))
(define goal+player-pict (cc-superimpose goal-pict player-alone-pict))

(define (sokoban->pict soko)
  (apply
   vl-append
   (for/list ([row (in-range (board-n-rows soko))])
     (apply
      ht-append
      (for/list ([col (in-range (board-n-cols soko))])
        (define c (board-ref soko row col))
        (cond [(= c empty-cell) empty-cell-pict]
              [(= c wall) wall-pict]
              [(= c goal) goal-pict]
              [(= c box) box-pict]
              [(= c player) player-pict]
              [(= c goal+box) goal+box-pict]
              [(= c goal+player) goal+player-pict]
              [else (error "Unknown cell" c)]))))))
