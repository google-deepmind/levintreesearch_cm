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

(require racket/fixnum
         racket/list
         racket/format
         "encode.rkt"
         define2)

(provide (all-defined-out))

;=============;
;=== Board ===;
;=============;

;;; A board is internally a byte string, so it can hold values between 0 and 255 only.
;;; This has the advantage of potentially being more efficient to process and store.

;; vec: bytes?
(struct board (vec n-rows n-cols)
  #:transparent)

(define drows '(-1 0 1 0)) ; up left down right
(define dcols '(0 -1 0 1))

; 4 is no-action
(define (action->string act)
  (case act [(0) "↑"] [(1) "←"] [(2) "↓"] [(3) "→"] [(4) "∗"] [else "?"]))

;; Returns the index in the vector corresponding to (row, col)
(define-syntax-rule (board-index aboard row col)
  (fx+ (fx* row (board-n-cols aboard)) col))

(define (make-board #:? [constructor board] n-rows n-cols val . other-args)
  (apply constructor (make-bytes (* n-rows n-cols) val) n-rows n-cols other-args))

(define (board-copy brd)
  (board (bytes-copy (board-vec brd)) (board-n-rows brd) (board-n-cols brd)))

(define (board-ref aboard row col)
  (bytes-ref (board-vec aboard) (board-index aboard row col)))

(define (board-in-bounds? brd row col)
  (and (< -1 row (board-n-rows brd))
       (< -1 col (board-n-cols brd))))

(define (board-set! aboard row col val)
  (bytes-set! (board-vec aboard)
              (board-index aboard row col)
              val))

;; lst is truncated to the closest board size
(define (list->board lst n-cols)
  (define n-rows (quotient (length lst) n-cols))
  (define l2 (take lst (* n-rows n-cols)))
  (board (apply bytes l2) n-rows n-cols))

(define (board->list aboard)
  (bytes->list (board-vec aboard)))

(define (board->bytes aboard)
  (board-vec aboard))

(define (board-find aboard x)
  (for/fold ([a #f] [b #f]) ; because for/or and for/first return only single values
            ([y (in-bytes (board-vec aboard))]
             [pos (in-naturals)]
             #:when (= x y))
    (quotient/remainder pos (board-n-cols aboard))))

(define (print-board aboard)
  (for ([row (in-range (board-n-rows aboard))])
    (for ([col (in-range (board-n-cols aboard))])
      (display (~a (board-ref aboard row col) #:min-width 4 #:align 'right)))
    (newline)))

;================;
;=== Contexts ===;
;================;

;; brd : byte-board
;; collect! : fixnum -> any/c
;; max-value : byte ; maximum value that a cell of the board can take
;; pad-value : byte ; value assigned to cells outside of the board.
;;
;; For each tile of the tiling (one tile is one mutex set),
;; calculates a context code base on values of the cells in the board
;; and calls `collect!` with this code.
;; `collect!` is meant to store the context code in some external structure
;; (or ignore it).
;; A tile is a rectangle of size row-span × col-span, whose top-left corner
;; is at some distance (bounded by row-dist, col-dist) from (row0, col0).
;;
;; Has side effects if `collect!` has side effects.
;; The function returns (void).
(define (board-relative-tiling/collect brd
                                      #:! collect!
                                      #:row row0 #:col col0
                                      #:? [max-value 255] #:? [pad-value max-value]
                                      #:? [row-dist 1] #:? [col-dist row-dist]
                                      #:? [row-span 2] #:? [col-span row-span])
  (define n-rows (board-n-rows brd))
  (define n-cols (board-n-cols brd))
  (define n-row-pos (fx+ row-dist row-dist 2 (fx- row-span)))
  (define n-col-pos (fx+ col-dist col-dist 2 (fx- col-span)))
  ;; + 1 for the center cell, and +1 because actually (- width 1)
  ;; For each tile:
  (for* ([row (in-range (fx- row0 row-dist) (fx+ row0 2 (fx- row-dist row-span)))]
         [col (in-range (fx- col0 col-dist) (fx+ col0 2 (fx- col-dist col-span)))])
    (define enc
      ;; Calculate the context code
      (for*/fold ([enc 0])
                 ([r (in-range row (fx+ row row-span))]
                  [c (in-range col (fx+ col col-span))])
        (define x
          (if (and (fx< -1 r n-rows)
                   (fx< -1 c n-cols))
              (board-ref brd r c) ; could be inlined for efficiency
              pad-value))
        (natural-encode1 x (+ max-value 1) enc)))
    (collect! enc)))
