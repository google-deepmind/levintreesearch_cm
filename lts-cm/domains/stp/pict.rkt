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
         "sliding-tile.rkt"
         define2)

(provide stp->pict)

(define cell-size 32)

(define empty-cell-pict
  (filled-rectangle cell-size cell-size #:color "white" #:border-color "black" #:border-width 1))

(define (make-cell n)
  (cc-superimpose empty-cell-pict
                  (text (number->string n))))

(define (stp->pict stp)
  (apply
   vl-append
   (for/list ([row (in-range (board-n-rows stp))])
     (apply
      ht-append
      (for/list ([col (in-range (board-n-cols stp))])
        (define c (board-ref stp row col))
        (if (= 0 c)
            empty-cell-pict
            (make-cell c)))))))

(module+ drracket
  (stp->pict (make-stp 5)))
