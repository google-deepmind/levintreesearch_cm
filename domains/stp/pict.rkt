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

(require lts-cm/byte-board
         pict
         racket/draw
         "sliding-tile.rkt"
         define2)

(provide stp->pict)

(define cell-size 32)

(define (empty-cell-pict color)
  (filled-rectangle cell-size cell-size #:color color #:border-color "black" #:border-width 1))

(define (get-cell-color n n-rows n-cols)
  (define-values (row col) (quotient/remainder n n-cols))
  (make-color 180
              (quotient (* 256 row) n-rows)
              (quotient (* 256 col) n-cols)))

(define (make-cell n n-rows n-cols)
  (cc-superimpose (empty-cell-pict (get-cell-color n n-rows n-cols))
                  (text (number->string n))))

(define (stp->pict stp)
  (define n-rows (board-n-rows stp))
  (define n-cols (board-n-cols stp))
  (apply
   vl-append
   (for/list ([row (in-range n-rows)])
     (apply
      ht-append
      (for/list ([col (in-range n-cols)])
        (define c (board-ref stp row col))
        (if (= 0 c)
            (empty-cell-pict "white")
            (make-cell c n-rows n-cols)))))))

(module+ drracket
  (stp->pict (make-stp 5)))
