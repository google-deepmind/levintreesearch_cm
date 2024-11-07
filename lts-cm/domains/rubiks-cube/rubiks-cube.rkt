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

(require racket/dict
         racket/list
         lts-cm/bfs
         lts-cm/encode
         lts-cm/misc
         lts-cm/byte-board
         define2)

(provide (all-defined-out))

;**************************************************************************************;
;****                                 Rubik's Cube                                 ****;
;**************************************************************************************;

#|
The state is a single vector of 20 elements.
Each of the first 8 elements is a corner index and its orientation (1 in 3).
Eachs of the last 12 elements is a side index with its orientation (1 in 2).


            /  4 16  5/
       4   / 12    13/      5
   12 19  / 0  8  1 /   13 17
 0     7  ———————— /  1     6
11 15    | 0  8  1 |  9 14
 3       |11     9 |  2
         | 3 10  2 |
          —————————
            7 18  6
          15    14
         3 10  2

Initially all cubes are oriented at 0, which correspond to facing Up for the corner cubes
of the or Up face, or Down for the others.
If a cubie is oriented 0, then rotations D and U (and D' and U') preserve this orientation,
but switch between the other 2; that is, if the orientation is 1, then the new orientation is 2
and vice-versa.
F and B preserve orientation 1 (and toggle the other orientations).
R and L preserve orientation 2.
[Then in its initial position, the sticker facing U or D is 0, the sticker facing F or B is 1,
and the sticker facing R or L is 2.
In a random position, if the sticker 0 faces U or D then the orientation is 0,
 if it faces F or B then the orientation is 1, and if it faces R or L it is 2.
]

It also means that if a cubie is oriented 1, then its sticker that's facing Up or Down in
the solved position is now facing either Front (if it's located on the Front face) or Back
(if located on the Back face).
Similarly, if it's oriented 2, then the corresponding sticker is facing Right or Left.

For the sides, there are only 2 orientations.
Every rotation changes the orientation of a rotated cubie.
The X mark the initial orientations of the side cubies (+ other faces by symmetry):
   ...
  X.X   .
 ...  . X
.X. . . .
... X .
.X. .

The sticker matching the X in the initial position is the (+) sticker.
For a random state, if the (+) sticker of side cubie matches the X, then the cubie is oriented 0,
otherwise it is oriented 1.
Every rotation changes the orientation from 0 to 1 or conversely.

|#

(define n-actions 12)
(define n-cubies 20)
(define n-corners 8)
(define n-sides 12)

(define (reverse-action act)
  (modulo (+ act 6) 12))

(define (reverse-act-seq acts)
  (map reverse-action (reverse acts)))

;; single byte vector of length 20
(struct cube (bytes)
  #:transparent)

(define (bytes->cube bts)
  (cube bts))

(define (cubie+orientation->number c o)
  ;; For simplicity we also consider 3 orientations for sides, even though one is not used.
  (naturals->fixnum* [c n-cubies] [o 3]))

(define (number->cubie+orientation n)
  (apply values (fixnum->naturals n '(20 3))))

(define (make-solved-cube)
  (define bts (make-bytes n-cubies 0))
  (for ([c (in-range n-cubies)])
    ;; Initially all orientations are 0
    (bytes-set! bts c (cubie+orientation->number c 0)))
  (cube bts))

(define (cube-copy cub)
  (cube (bytes-copy (cube-bytes cub))))

(define all-transf
  #(#(#(#(4 5 1 0 4) #(16 13 8 12 16))  #(#(0 2 1) #(1 0)))
    #(#(#(0 1 2 3 0) #(8 9 10 11 8))    #(#(2 1 0) #(1 0)))
    #(#(#(1 5 6 2 1) #(13 17 14 9 13))  #(#(1 0 2) #(1 0)))
    #(#(#(7 6 5 4 7) #(19 18 17 16 19)) #(#(2 1 0) #(1 0)))
    #(#(#(0 3 7 4 0) #(12 11 15 19 12)) #(#(1 0 2) #(1 0)))
    #(#(#(6 7 3 2 6) #(14 18 15 10 14)) #(#(0 2 1) #(1 0)))))

(define all-idx-transf
  (for/vector ([x all-transf])
    (vector-ref x 0)))

;; Create a transformation matrix to speed up computation.
;; This actually saves only very little.
(define vtrans
  (let ()
    (define vtrans (make-board (* 20 3) 6 0))
    (for ([cubie (in-range 20)]
          #:when #t
          [orien (if (< cubie 8) (in-range 3) (in-range 2))]
          #:when #t
          [face-to-rotate (in-range 6)])
      (define transf (vector-ref all-transf face-to-rotate))
      (define n (cubie+orientation->number cubie orien))
      (define or2 (vector-ref (vector-ref (vector-ref transf 1)
                                          (if (< cubie 8) 0 1))
                              orien))
      (define n2 (cubie+orientation->number cubie or2))
      (board-set! vtrans n face-to-rotate n2))
    vtrans))

;; Returns cube2, which by default is a copy of cub
;; `cub2` MUST NOT be eq? to `cub`
(define (do-action cub action #:cube2 [cub2 (cube-copy cub)])
  (define bts-from (cube-bytes cub))
  (define bts-to   (cube-bytes cub2))

  ;; The action is the face to rotate + clockwise or not
  (define-values (face-to-rotate clockwise?)
    (if (>= action 6)
        (values (- action 6) #false)
        (values action #true)))

  (define idx-transf (vector-ref all-idx-transf face-to-rotate))

  (for ([idxs  (in-vector idx-transf)])
    (for ([idx1 (in-vector idxs)]
          [idx2 (in-vector idxs 1)])
      (define-values (idx-from idx-to)
        (if clockwise? (values idx1 idx2) (values idx2 idx1)))
      (define n (bytes-ref bts-from idx-from))
      (define n2 (board-ref vtrans n face-to-rotate))
      (bytes-set! bts-to idx-to n2)))

  cub2)

(define (move* cub acts)
  (for/fold ([cub cub])
            ([act (in-list acts)])
    (do-action cub act)))

(define (scramble-cube cub n)
  (move* cub (build-list n (λ (i) (random n-actions)))))

(define (make-random-cube n-scrambles)
  (scramble-cube (make-solved-cube) n-scrambles))

(define solved-cube-bytes (cube-bytes (make-solved-cube)))

(define (cube-solution? cub)
  ;; Not very efficient. We could keep track of how many cells or cubelets are correct
  ;; after each move
  (bytes=? (cube-bytes cub) solved-cube-bytes))

;================;
;=== Contexts ===;
;================;

(define state-contexts
  ;; Turn into a vector of fxvector for speed/memory
  (list-of-lists->vector-of-fxvectors
   ;; This can only give less than (* 24 24 190)=109440 different contexts, which is not much
   ;; In pratice, 104256 contexts are create (very quickly, then staves off)
   (combinations (range n-cubies) 2)))

(define (get-contexts/setter cub nd set-next-context!)
  (define bts (cube-bytes cub))

  ;; (* n-cubies 3) is wasteful, it should be 24=8*3=12*2 instead, but position and orientation
  ;; are not encoded within 24 values only.
  (define max-val (- (* n-cubies 3) 1))
  (for ([idxs (in-vector state-contexts)])
    ;; Get the values of the indices idxs in bts, and encode them into a single fixnum
    (set-next-context! (bytes-context/encode bts idxs #:max-value max-val)))

  ;;  Last action
  (set-next-context!
   ;; No encoding necessary since there's only one number
   (bfs-node-last-action nd #:no-action n-actions)))

;==============;
;=== Search ===;
;==============;

(define (get-visited-key cub _nd)
  (cube-bytes cub))

(define solve-cube
  (make-bfs-solver #:n-actions n-actions
                   #:get-contexts/setter get-contexts/setter
                   #:get-visited-key get-visited-key
                   #:solution? cube-solution?
                   #:do-action do-action))

;=============;
;=== Mains ===;
;=============;

(module+ main
  (require lts-cm/worker)
  (domain-main #:spec->state bytes->cube #:solver solve-cube))

(module+ worker
  (require lts-cm/worker)
  (domain-worker #:spec->state bytes->cube #:solver solve-cube))

;================;
;=== For Pict ===;
;================;

#; ; The indices of a face are as such:
(0 1 2
 3 4 5
 6 7 8)

;; The order of the corners matches the 8 first cubies in a cube bytes.
;; The first position is the default orientation, then it goes in clockwise order.
(define corners
  '((ufl . ((0 6) (1 0) (4 2))) ; cubie at (up, front, left); oriented up, then front then left
    (urf . ((0 8) (2 0) (1 2)))
    (dfr . ((5 2) (1 8) (2 6)))
    (dlf . ((5 0) (4 8) (1 6)))
    (ulb . ((0 0) (4 0) (3 2)))
    (ubr . ((0 2) (3 0) (2 2)))
    (drb . ((5 8) (2 8) (3 6)))
    (dbl . ((5 6) (3 8) (4 6)))))

;; The order of the edges matches the 12 last cubies in a cube bytes
;; The order of the positions for each cubicle is also important as it defines orientation.
(define edges
  '((uf . ((0 7) (1 1))) ; oriented up
    (fr . ((1 5) (2 3))) ; oriented front
    (df . ((5 1) (1 7))) ; oriented down
    (fl . ((1 3) (4 5))) ; oriented front

    (ul . ((4 1) (0 3))) ; oriented left
    (ur . ((2 1) (0 5))) ; oriented right
    (dr . ((2 7) (5 5))) ; oriented right
    (dl . ((4 7) (5 3))) ; oriented left

    (ub . ((0 1) (3 1))) ; oriented up
    (br . ((3 3) (2 5))) ; oriented back
    (db . ((5 7) (3 7))) ; oriented down
    (bl . ((3 5) (4 3))))) ; oriented back


;; Notice: the order in cubies matches the order in the cube board
(define cubicles (append corners edges))

;; Converts a `cube` into a representation suitable for pict
(define (cube->face-boards cub)
  (define face-boards (build-list 6 (λ (face) (make-board 3 3 face))))
  (for ([cubie+or (in-bytes (cube-bytes cub))]
        [(name stickers) (in-dict cubicles)])
    (define-values (cubie orientation) (number->cubie+orientation cubie+or))
    (define towards (vector-ref #((0 5) (1 3) (2 4)) orientation))
    (define colors (map car (cdr (list-ref cubicles cubie))))
    (define ordered-stickers
      (if (< cubie 8)
          ;; corner
          (memf (λ (st) (member (first st) towards))
                (append stickers stickers))
          ;; edge
          (if (= 0 orientation) stickers (reverse stickers))))
    (assert ordered-stickers cubie orientation towards colors stickers)
    (for ([face+pos (in-list ordered-stickers)]
          [color (in-list colors)])
      (define bts (board-vec (list-ref face-boards (first face+pos))))
      (bytes-set! bts (second face+pos) color)))
  face-boards)

;=============;
;=== Tests ===;
;=============;

(module+ test
  (require rackunit)
  (define init-cube (make-random-cube (random 100)))

  ;; Check all reverse actions
  (for ([i 12])
    (check-equal? (do-action (do-action init-cube i) (modulo (+ i 6) 12))
                  init-cube
                  (format "action: ~a" i)))

  ;; Check random actions sequences and their inverses
  (let ()
    (define acts (build-list (+ 1 (random 20)) (λ _ (random n-actions))))
    (check-equal? (move* (move* init-cube acts) (reverse-act-seq acts))
                  init-cube))

  ;; Check the final state of a fixed random sequence of actions + all actions
  (check-equal?
   (move* (make-solved-cube) '(6 1 7 3 5 4 8 4 7 7 6 9 1 7 5 3 9 11 6 7 0 1 2 3 4 5 6 7 8 9 10 11))
   (cube #"\20\2\5\r\27\6\v\23\0344\31+!\37%(1:7-"))

  ;; Test bfs, 20736 act seqs of length 4
  (check-eq? (dict-ref (solve-cube (make-random-cube 4) #:budget 100000) 'status)
               'SOLVED))
