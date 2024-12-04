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

(require lts-cm/bfs
         lts-cm/encode
         lts-cm/byte-board
         lts-cm/context-setter
         racket/dict
         racket/string
         racket/match
         racket/list
         define2)

(provide (all-defined-out))

;***************************************************************************************;
;****                                  The Witness                                  ****;
;***************************************************************************************;

(define n-actions 4)
(define max-color 4) ; used for contexts. Increase if needed.

(define NO_SEG #b00)
(define V_SEG  #b01)
(define H_SEG  #b10)
(define HV_SEG #b11)
#;(define GOAL_SEG #b100) ; apparently the goal is not even necessary!
(define dots-max-value HV_SEG #;GOAL_SEG)

;; row0, col0: tip of the snake
;; rowg, colg: goal position in dots
(struct witness (colors dots [row0 #:mutable] [col0 #:mutable] rowg colg)
  #:transparent)

;; Convert a problem read from the .rktd files to a witness struct
(define (spec->witness spec)
  (match spec
    [(list (list n-rows n-cols) (list row0 col0) (list rowg colg) colors)
     (define dots (make-board (+ n-rows 1) (+ n-cols 1) 0))

     (witness (board (list->bytes colors) n-rows n-cols) dots row0 col0 rowg colg)]
    [else
     (error "no matching case" spec)]))

;; Also used by gui.rkt to generate a picture.
(define (witness->string wit)
  (define dots (witness-dots wit))
  (define colors (witness-colors wit))
  (define n-rows (board-n-rows dots))
  (define n-cols (board-n-cols dots))
  (string-append*
   (for/list ([r (in-range n-rows)])
     ;; inter row
     (define inter-strs
       (for/list ([c (in-range n-cols)])
         ;; inter row and inter col
         (define bits 0)
         (define n-set 0)
         (for ([dr (in-list '(-1 0 0 0))]
               [dc (in-list '(0 -1 0 0))]
               [seg_check (list V_SEG H_SEG V_SEG H_SEG)] ; up left down right
               [i (in-range 3 -1 -1)])
           (define r2 (+ r dr))
           (define c2 (+ c dc))
           (when (and (board-in-bounds? dots r2 c2)
                      (> (bitwise-and seg_check (board-ref dots r2 c2)) 0))
             (set! bits (bitwise-ior bits (arithmetic-shift 1 i)))
             (set! n-set (+ n-set 1))))
         (define ch
           (case bits
             [(#b1010) "│"]
             [(#b0101) "─"]
             [(#b1100) "┘"]
             [(#b0110) "┐"]
             [(#b0011) "┌"]
             [(#b1001) "└"]
             [else (cond [(> n-set 2)
                          (error "More than 2 bits set at " r c dots)]
                         [(and (= r (witness-rowg wit)) (= c (witness-colg wit)))
                          "*"]
                         [else " "])]))
         (string-append ch (if (> (bitwise-and H_SEG (board-ref dots r c)) 0) "─" " "))))
     (define row-strs
       (for/list ([c (in-range n-cols)])
         (define inter
           (if (> (bitwise-and V_SEG (board-ref dots r c)) 0)
               "│"
               " "))
         (define color
           (if (board-in-bounds? colors r c)
               (number->string (board-ref colors r c))
               " "))
         (string-append inter color)))
     (string-append (string-append* inter-strs)
                    "\n"
                    (string-append* row-strs)
                    "\n"))))

(define (print-witness wit)
  (printf "goal: ~a ~a \n" (witness-rowg wit) (witness-colg wit))
  (displayln (witness->string wit)))

(define (move! wit act)
  (define row (witness-row0 wit))
  (define col (witness-col0 wit))
  (define drow (list-ref drows act))
  (define dcol (list-ref dcols act))
  (define row2 (+ drow row))
  (define col2 (+ dcol col))
  (define dots (witness-dots wit))
  (cond [(and (board-in-bounds? dots row2 col2)
              ;; Neither H_SEG or V_SEG must start in (row2, col2)
              (= (board-ref dots row2 col2) NO_SEG)
              ;; Check that no segment ends in (row2, col2)
              (let ([up   (if (> row2 0) (board-ref dots (- row2 1) col2) NO_SEG)]
                    [left (if (> col2 0) (board-ref dots row2 (- col2 1)) NO_SEG)])
                (= 0 (bitwise-ior (bitwise-and up V_SEG)
                                  (bitwise-and left H_SEG)))))
         (case act
           [(0) ; up
            ;; Vertical segments are always from up to down
            (board-set! dots row2 col2 (bitwise-ior V_SEG (board-ref dots row2 col2)))]
           [(1) ; left
            ;; Horizontal segments are always from left to right
            (board-set! dots row2 col2 (bitwise-ior H_SEG (board-ref dots row2 col2)))]
           [(2) ; down
            ;; Vertical segments are always from up to down
            (board-set! dots row col (bitwise-ior V_SEG (board-ref dots row col)))]
           [(3) ; right
            ;; Horizontal segments are always from left to right
            (board-set! dots row col (bitwise-ior H_SEG (board-ref dots row col)))])
         (set-witness-row0! wit row2)
         (set-witness-col0! wit col2)]
        [else
         ;; No movement, no change
         #false]))

(define (witness-solution? wit)
  (let/ec return
    (unless (and (= (witness-rowg wit) (witness-row0 wit))
                 (= (witness-colg wit) (witness-col0 wit)))
      ;; Snake not at exit position
      (return #false))
    (define colors (witness-colors wit))
    (define n-rows (board-n-rows colors))
    (define n-cols (board-n-cols colors))
    (define dots (witness-dots wit))
    (define visited-colors (board-copy colors))

    (define to-visit (for*/list ([r (in-range n-rows)]
                                 [c (in-range n-cols)]
                                 #:do [(define color (board-ref colors r c))]
                                 #:unless (= color 0))
                       (list r c color)))
    (let loop ([to-visit to-visit])
      (when (empty? to-visit) (return #true))
      (define-values (row col color) (apply values (first to-visit)))
      (set! to-visit (rest to-visit))
      (for ([drow (in-list drows)] ; up left down right
            [dcol (in-list dcols)]
            [test-seg (in-list (list H_SEG V_SEG H_SEG V_SEG))])
        (define row2 (+ row drow))
        (define col2 (+ col dcol))
        (when (board-in-bounds? colors row2 col2)
          (define seg (board-ref dots (max row row2) (max col col2)))
          ;; If a segment separates the two cells, skip.
          (unless (or (= seg test-seg)
                      (= seg HV_SEG))
            (define neighbour-color (board-ref visited-colors row2 col2))
            (cond [(= neighbour-color 0)
                   (board-set! visited-colors row2 col2 color)
                   (set! to-visit (cons (list row2 col2 color) to-visit))]
                  [(not (= neighbour-color color))
                   (return #false)]))))
      (loop to-visit))))

;================;
;=== Contexts ===;
;================;

(define pad-color (+ max-color 1))
(define pad-dot HV_SEG)
(define max-dot (max dots-max-value pad-dot))

(define (collect-contexts wit sstate set-next-context!)
  (match-define (witness colors dots row0 col0 rowg colg) wit)

  ;; Relative tiling of the two boards (dots and colors) at the same time, as if they
  ;; were just one board.
  ;; It might be simpler just to copy the whole thing into a single board though...
  (define (set-witness-relative-tiling-contexts! #:! row-dist #:! col-dist #:! row-span #:! col-span)
    ;; Collect the codes for the dots board only
    (define dots-codes (make-list-setter))
    (board-relative-tiling/setter
          dots
          #:setter! dots-codes
          #:row-dist row-dist #:col-dist col-dist #:row-span row-span #:col-span col-span
          #:row row0 #:col col0 #:max-value max-dot #:pad-value pad-dot)
    ;; Collect the codes for the colors board only
    (define colors-codes (make-list-setter))
    (board-relative-tiling/setter
     colors
     #:setter! colors-codes
     #:row-dist row-dist #:col-dist col-dist #:row-span row-span #:col-span col-span
     #:row row0 #:col col0 #:max-value pad-color #:pad-value pad-color)
    ;; Number of possible codes for each of the colors-codes below.
    (define color-code-size (expt (+ pad-color 1) (* col-span row-span)))
    ;; Merge the codes 2 by 2
    (for ([dots-code   (in-list (dots-codes))]
          [colors-code (in-list (colors-codes))])
      (set-next-context! (naturals->fixnum* dots-code [colors-code color-code-size]))))

  ;; Encode the position of the goal
  (set-next-context! (naturals->fixnum* [rowg (board-n-rows dots)] [colg (board-n-cols dots)]))

  (set-witness-relative-tiling-contexts! #:row-dist 4 #:col-dist 4 #:row-span 3 #:col-span 3)
  (set-witness-relative-tiling-contexts! #:row-dist 4 #:col-dist 4 #:row-span 2 #:col-span 2)
  (set-witness-relative-tiling-contexts! #:row-dist 1 #:col-dist 1 #:row-span 2 #:col-span 1)
  (set-witness-relative-tiling-contexts! #:row-dist 1 #:col-dist 1 #:row-span 1 #:col-span 2))

;==============;
;=== Search ===;
;==============;

;; The copy should not copy the colors, only the dots and positions.
(define (witness-copy wit)
  (match-define (witness colors dots row0 col0 rowg colg) wit)
  (witness colors (board-copy dots) row0 col0 rowg colg))

;; Returns a new state
(define (do-action wit act)
  (define wit2 (witness-copy wit))
  (move! wit2 act)
  wit2)

(define (get-visited-key wit _nd)
  (board->bytes (witness-dots wit))) ; not very efficient but good enough for a start

(define solve-witness
  (make-bfs-solver #:n-actions n-actions
                   #:collect-contexts collect-contexts
                   #:get-visited-key get-visited-key
                   #:solution? witness-solution?
                   #:do-action do-action))

;=============;
;=== Mains ===;
;=============;

(module+ main
  (require lts-cm/worker)
  (domain-main #:spec->state spec->witness #:solver solve-witness))

(module+ worker
  (require lts-cm/worker)
  (domain-worker #:spec->state spec->witness #:solver solve-witness))

;=============;
;=== Tests ===;
;=============;

(module+ test
  (require racket/dict
           rackunit)

  (define spec1 '((4 4) (0 0) (4 2) (4 2 0 0 4 0 2 2 0 0 2 2 0 0 0 0)))

  (define (sym->act sym)
    (case sym [(up) 0] [(left) 1] [(down) 2] [(right) 3]))

  (for ([(sol? acts)
         (in-dict (list '(#true  down down right up up right right down down down left down)
                        '(#false down down right up up right right down down down left down right)
                        '(#false down down right up up right right down down down left)
                        '(#true  right down down down down right)
                        '(#false right right down down down down)))])
    (define wit (spec->witness spec1))
    (for ([act (map sym->act acts)])
      (move! wit act))
    (print-witness wit)
    (check-equal? (witness-solution? wit) sol?))

  (let ()
    (define wit (spec->witness spec1))
    (define set-next-context! (make-list-setter))
    (collect-contexts wit #f set-next-context!)
    (printf "#contexts: ~a\n" (length (set-next-context!)))
    (writeln (set-next-context!))
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
    (check-equal? rc-seq
                  '((1 0) (1 1) (2 1) (2 0) (3 0) (3 1) (3 2) (2 2)
                          (2 2) ; illegal move
                          (1 2) (1 2) ; illegal
                          (1 3) (1 4) (0 4) (0 3) (0 3) ; illegal
                          (0 3)
                          (0 2) (0 2)
                          (0 1) (0 1)
                          (0 1) ; can't go back to start
                          ))))
