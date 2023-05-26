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
         racket/string
         pict
         "witness.rkt"
         define2)

(provide witness->pict)

(define cell-size 32)

(define empty-cell-pict
  (filled-rectangle cell-size cell-size #:color "white" #:border-color "white" #:border-width 1))

(define (make-cell n)
  (cc-superimpose empty-cell-pict
                  (text (number->string n))))

(define colors '("white" "red" "green" "blue" "orange" "cyan" "purple" "magenta" "lightgreen"))

(define (witness->pict wit)
  (define rows (string-split (witness->string wit) "\n"))
  (frame
   (apply
    vl-append
    (for/list ([row (in-list rows)])
      (apply
       ht-append
       (for/list ([c (in-string row)])
         (case c
           [(#\space) empty-cell-pict]
           [(#\│) (cc-superimpose empty-cell-pict
                                  (filled-rectangle 2 cell-size #:color "black"))]
           [(#\─) (cc-superimpose empty-cell-pict
                                  (filled-rectangle cell-size 2 #:color "black"))]
           [(#\┘) (ct-superimpose
                   (lc-superimpose empty-cell-pict
                                   (filled-rectangle (/ cell-size 2) 2 #:color "black"))
                   (filled-rectangle 2 (/ cell-size 2) #:color "black"))]
           [(#\┐) (cb-superimpose
                   (lc-superimpose empty-cell-pict
                                   (filled-rectangle (/ cell-size 2) 2 #:color "black"))
                   (filled-rectangle 2 (/ cell-size 2) #:color "black"))]
           [(#\┌) (cb-superimpose
                   (rc-superimpose empty-cell-pict
                                   (filled-rectangle (/ cell-size 2) 2 #:color "black"))
                   (filled-rectangle 2 (/ cell-size 2) #:color "black"))]
           [(#\└) (ct-superimpose
                   (rc-superimpose empty-cell-pict
                                   (filled-rectangle (/ cell-size 2) 2 #:color "black"))
                   (filled-rectangle 2 (/ cell-size 2) #:color "black"))]
           [(#\*) (cc-superimpose empty-cell-pict
                                  (text "*"))]
           [else (cc-superimpose empty-cell-pict
                                 (filled-rectangle cell-size cell-size
                                                   #:color (list-ref colors
                                                                     (- (char->integer c)
                                                                        (char->integer #\0)))
                                                   #:border-color "white"
                                                   #:border-width 1))])))))))

(module+ drracket
  (define spec1 '((4 4) (0 0) (4 2) (4 2 0 0 4 0 2 2 0 0 2 2 0 0 0 0)))
  (define (sym->act sym)
    (case sym [(up) 0] [(left) 1] [(down) 2] [(right) 3]))
  (define wit (spec->witness spec1))
  (witness->pict wit)
  (define acts '(down right down left down right right up
                        left ; illegal move
                        up left ; illegal
                        right right up left down ; illegal
                        up ; out of bounds
                        left down
                        left down
                        left
                        ))
    (define rc-seq
      (for/list ([act acts])
        (move! wit (sym->act act))
        (list (witness-row0 wit) (witness-col0 wit))))
  (witness->pict wit))
