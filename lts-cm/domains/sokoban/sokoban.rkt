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

(require racket/performance-hint
         racket/dict
         racket/list
         racket/string
         racket/fixnum
         lts-cm/bfs
         lts-cm/byte-board
         lts-cm/misc
         define2)

(provide (all-defined-out)
         n-actions)

;***************************************************************************************;
;****                                    Sokoban                                    ****;
;***************************************************************************************;

(define n-actions 4) ; 0=up 1=left 2=down 3=right

(define empty-cell  #b0000)
(define goal        #b0001)
(define box         #b0010)
(define goal+box    #b0011)
(define player      #b0100)
(define goal+player #b0101)
(define wall        #b1000)

(define max-cell (max empty-cell wall goal box player goal+box goal+player))

(define-inline (has-goal? c)
  (bitwise-bit-set? c 0))
(define-inline (has-box? c)
  (bitwise-bit-set? c 1))
(define-inline (has-player? c)
  (bitwise-bit-set? c 2))

;; undo-action: (or/c 0 1 2 3 4)
;; 4 is for no inverse/undo action. Playing this action undoes the previous action
;;   Must be initialized to 4.
(struct sokoban board ([player-row #:mutable]
                       [player-col #:mutable]
                       [n-solved #:mutable]
                       n-boxes
                       [undo-action #:mutable])
  #:transparent)

(define (sokoban-copy soko)
  (sokoban (bytes-copy (board-vec soko))
           (board-n-rows        soko)
           (board-n-cols        soko)
           (sokoban-player-row  soko)
           (sokoban-player-col  soko)
           (sokoban-n-solved    soko)
           (sokoban-n-boxes     soko)
           (sokoban-undo-action soko)))

(define (normalize-line line n-cols)
  (define m (regexp-match-positions #px"^ *(#.*#) *$|^ *(#) *$" line))
  (define p (or (second m) (third m)))
  (unless p (error "cannot normalize line: " line n-cols))
  (string-append
   (make-string (car p) #\#)
   (substring line (car p) (cdr p))
   (make-string (- n-cols (cdr p)) #\#)))

;; Builds a sokoban struct from a string representing a sokoban level in standard notation,
;; and normalizes the lines by padding the lhs and rhs with walls.
(define (string->sokoban str)
  (define l (string-split (string-trim str #px" *\n") "\n"))
  (define n-rows (length l))
  (define n-cols (apply max (map string-length l)))
  ;; fill with walls on the lhs and rhs.
  ;; http://www.sokobano.de/wiki/index.php?title=Level_format#Level
  (define vec
    (apply
     bytes
     (for*/list ([row (in-list l)]
                 #:do [(define row2 (normalize-line row n-cols))]
                 [c (in-string row2)])
       (case c
         [(#\space) empty-cell]
         [(#\#) wall]
         [(#\$) box]
         [(#\@) player]
         [(#\.) goal]
         [(#\*) goal+box]
         [(#\+) goal+player]
         [else (error "Unknown character:" c)]))))
  (define n-boxes
    (for/sum ([c (in-bytes vec)])
      (if (has-box? c) 1 0)))
  (define n-solved
    (for/sum ([c (in-bytes vec)])
      (if (fx= c goal+box) 1 0)))

  (define soko (sokoban vec n-rows n-cols 0 0 n-solved n-boxes n-actions))
  (define-values (prow pcol) (find-player soko))
  (set-sokoban-player-row! soko prow)
  (set-sokoban-player-col! soko pcol)
  soko)

(define (sokoban->string soko)
  (string-join
   (for/list ([row (in-range (board-n-rows soko))])
     (apply
      string
      (for/list ([col (in-range (board-n-cols soko))])
        (define c (board-ref soko row col))
        (cond [(= c empty-cell)  #\space]
              [(= c wall)        #\#]
              [(= c box)         #\$]
              [(= c player)      #\@]
              [(= c goal)        #\.]
              [(= c goal+box)    #\*]
              [(= c goal+player) #\+]
              [else              #\_]))))
   "\n"))

(define (find-player soko)
  (define-values (r c) (board-find soko player))
  (if r
      (values r c)
      (board-find soko goal+player)))

(define (sokoban-solution? soko)
  (fx= (sokoban-n-boxes soko)
       (sokoban-n-solved soko)))

;; Modifies the soko board by moving the play by drow rows and dcol cols if possible.
;; Returns 'box if a box is moved, #false if the player is hitting a wall (no movement),
;; and #true if the player is moving freely (action can be undone).
(define (move! soko drow dcol)
  (define row (sokoban-player-row soko))
  (define col (sokoban-player-col soko))
  (define row2 (+ row drow))
  (define col2 (+ col dcol))
  (define c2 (board-ref soko row2 col2))
  (cond
    [(has-box? c2)
     ;; These cannot be out of bounds as boards MUST have a layer of surrounding walls.
     (define row3 (+ row2 drow))
     (define col3 (+ col2 dcol))
     (define c3 (board-ref soko row3 col3))
     (cond [(or (fx= c3 wall) (has-box? c3))
            ;; Cannot move.
            #false]
           [else
            ;; Can push the box.
            (define dscore ; Diff in score.
              (- (if (has-goal? c3) 1 0)
                 (if (has-goal? c2) 1 0)))
            (define c (board-ref soko row col))
            (board-set! soko row  col  (bitwise-xor c  player)) ; remove player
            (board-set! soko row2 col2 (bitwise-xor c2 player box)) ; remove box, add player
            (board-set! soko row3 col3 (bitwise-xor c3 box)) ; add box
            (set-sokoban-player-row! soko row2)
            (set-sokoban-player-col! soko col2)
            (set-sokoban-n-solved! soko (+ (sokoban-n-solved soko) dscore))
            'box])]
    [(fx= c2 wall)
     ;; Cannot move.
     #false]
    [else
     (define c (board-ref soko row col))
     (board-set! soko row  col  (bitwise-xor c  player)) ; remove player
     (board-set! soko row2 col2 (bitwise-xor c2 player)) ; add player
     (set-sokoban-player-row! soko row2)
     (set-sokoban-player-col! soko col2)
     #true]))

;; Like move! but returns a state copy. The undo action is recorded in the new state.
(define (do-action soko act)
  (define soko2 (sokoban-copy soko))
  (define drow (list-ref drows act)) ; not efficient
  (define dcol (list-ref dcols act))
  (define trace (move! soko2 drow dcol))
  (define new-undo-action
    (if (eq? trace #true) ; moved freely (no box pushed, no hitting wall)
        (modulo (+ act 2) 4)  ; inverse of current action
        n-actions)) ; no undo action
  (set-sokoban-undo-action! soko2 new-undo-action)
  soko2)

;================;
;=== Contexts ===;
;================;

(define (collect-contexts soko sstate set-next-context!)
  (define row0 (sokoban-player-row soko))
  (define col0 (sokoban-player-col soko))
  ;; Include which action would lead to undoing the last action.
  ;; When pushing a box, no such action exists.
  (set-next-context! (sokoban-undo-action soko)) ; no encoding necessary since single value

  (board-relative-tiling/setter soko
                                     #:setter! set-next-context!
                                     #:row-dist 4 #:col-dist 4
                                     #:row-span 3 #:col-span 3
                                     #:row row0 #:col col0 #:max-value max-cell #:pad-value wall)
  (board-relative-tiling/setter soko
                                     #:setter! set-next-context!
                                     #:row-dist 3 #:col-dist 2
                                     #:row-span 4 #:col-span 2
                                     #:row row0 #:col col0 #:max-value max-cell #:pad-value wall)
  (board-relative-tiling/setter soko
                                     #:setter! set-next-context!
                                     #:row-dist 2 #:col-dist 3
                                     #:row-span 2 #:col-span 4
                                     #:row row0 #:col col0 #:max-value max-cell #:pad-value wall)
  (board-relative-tiling/setter soko
                                     #:setter! set-next-context!
                                     #:row-dist 2 #:col-dist 2
                                     #:row-span 2 #:col-span 2
                                     #:row row0 #:col col0 #:max-value max-cell #:pad-value wall)
  (board-relative-tiling/setter soko
                                     #:setter! set-next-context!
                                     #:row-dist 1 #:col-dist 1
                                     #:row-span 1 #:col-span 2
                                     #:row row0 #:col col0 #:max-value max-cell #:pad-value wall)
  (board-relative-tiling/setter soko
                                     #:setter! set-next-context!
                                     #:row-dist 1 #:col-dist 1
                                     #:row-span 2 #:col-span 1
                                     #:row row0 #:col col0 #:max-value max-cell #:pad-value wall))

;==============;
;=== Search ===;
;==============;

;; Returns the minimum information of the state of the sokoban board: position of the player
;; and sorted positions of the boxes.
;; Currently this function handles board of at most 65535 cells
;; This function takes linear time with the size of the board, which is inefficient, but
;; good enough for small boards, and at least this is invariant to box labels.
;; To obtain ~linear time with the number of boxes instead, we would need to keep track of the
;; (sorted) boxes' positions during moves.
(define (sokoban->compact-state soko)
  (define n (+ 1 (sokoban-n-boxes soko))) ; player at pos 0
  (define bts (make-bytes (* 2 n) 0))
  (for/fold ([box-id 2])
            ([c (in-bytes (board->bytes soko))] [i (in-naturals)])
    (cond [(has-player? c)
           (define low (bitwise-and i 255))
           (define high (arithmetic-shift i -8))
           (bytes-set! bts 0 low)
           (bytes-set! bts 1 high)
           box-id]
          [(has-box? c)
           (define low (bitwise-and i 255))
           (define high (arithmetic-shift i -8))
           (bytes-set! bts box-id low)
           (bytes-set! bts (+ box-id 1) high)
           (+ box-id 2)]
          [else box-id]))
  bts)

(define (get-visited-key soko _nd)
  (sokoban->compact-state soko))

(define solve-sokoban
  (make-bfs-solver #:n-actions n-actions
                   #:collect-contexts collect-contexts
                   #:get-visited-key get-visited-key
                   #:solution? sokoban-solution?
                   #:do-action do-action))

;=============;
;=== Mains ===;
;=============;

(module+ main
  (require lts-cm/worker)
  (domain-main #:spec->state string->sokoban #:solver solve-sokoban))

(module+ worker
  (require lts-cm/worker)
  (domain-worker #:spec->state string->sokoban #:solver solve-sokoban))

;=============;
;=== Tests ===;
;=============;

;; To check the number of mutex sets:
(module+ drracket
  (require lts-cm/context-setter)
  (let ()
    (define s
      "\
###########
# $ $ @$. #
###########")
    (define soko1 (string->sokoban s))
    (define setter (make-list-setter))
    (collect-contexts soko1 #f setter)
    (printf "Number of contexts: ~a\n" (length (setter)))))

(module+ test
  (require rackunit)
  (let ()
    (define s
      "\
###########
# $ $ @$. #
###########")
    (define soko1 (string->sokoban s))
    (check = (sokoban-n-boxes soko1) 3)
    (check = (sokoban-n-solved soko1) 0)
    (check eq? 'box (move! soko1 0 1) (format "~v" soko1))
    (check = (sokoban-n-solved soko1) 1)
    (check eq? 'box (move! soko1 0 1) (format "~v" soko1))
    (check = (sokoban-n-solved soko1) 0)
    (check eq? #f (move! soko1 1 0) (format "~v" soko1))
    (check = (sokoban-n-solved soko1) 0)
    (check equal? (sokoban->string soko1) "###########\n# $ $   +$#\n###########")
    (check eq? #t (move! soko1 0 -1) (format "~v" soko1))
    (check = (sokoban-n-solved soko1) 0)
    (check eq? #t (move! soko1 0 -1) (format "~v" soko1))
    (check = (sokoban-n-solved soko1) 0)
    (check eq? #t (move! soko1 0 -1) (format "~v" soko1))
    (check eq? 'box (move! soko1 0 -1) (format "~v" soko1))
    (check eq? #f (move! soko1 0 -1) (format "~v" soko1))
    (check equal? (sokoban->string soko1) "###########\n# $$@   .$#\n###########"))

  (let ()
    (define s
      "\
###########
# $ $ @$. #
###########")
    (define soko1 (string->sokoban s))
    (define res (silent (solve-sokoban soko1)))
    ; this problem cannot be solved because there are more boxes than goals
    (check eq? (dict-ref res 'status) #f (format "~v" res))
    (define bts1 (bytes->list (sokoban->compact-state soko1)))
    (check equal? bts1 '(17 0 13 0 15 0 18 0) (format "~v" bts1)))

  (let ()
    (define s
      "\
###########
#     @$. #
###########")
    (define soko1 (string->sokoban s))
    (define res (silent (solve-sokoban soko1)))
    ; this problem cannot be solved because there are more boxes than goals
    (check-pred result-solved? res)))
