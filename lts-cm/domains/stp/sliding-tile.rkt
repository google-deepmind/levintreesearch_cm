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
         lts-cm/byte-board
         racket/fixnum
         racket/format
         racket/list
         text-table
         define2)

(provide (all-defined-out))

;***************************************************************************************;
;****                              Sliding Tile Puzzle                              ****;
;***************************************************************************************;

(define n-actions 4)

(struct stp-state board ([row0 #:mutable] [col0 #:mutable] [n-solved #:mutable])
  #:transparent)

(define (list->stp lst #:? [n-scrambles 0])
  (define N (integer-sqrt (length lst)))
  (define aboard (list->board lst N))
  (define-values (row0 col0) (board-find aboard 0))
  (define-values (new-row0 new-col0)
    (scramble! aboard row0 col0 n-scrambles))
  (stp-state (board-vec aboard) (board-n-rows aboard) (board-n-cols aboard)
               new-row0 new-col0 (stp-n-solved aboard)))

(define (make-stp N #:? n-scrambles)
  (list->stp (build-list (* N N) values) #:n-scrambles n-scrambles))

(define (move aboard row col row2 col2 #:? [update!? #f])
  (define c  (board-ref aboard row  col))
  (define c2 (board-ref aboard row2 col2))
  (when update!?
    (board-set! aboard row  col  c2)
    (board-set! aboard row2 col2 c))
  (define idx  (board-index aboard row  col))
  (define idx2 (board-index aboard row2 col2))
  ; return whether we have gained or lost solved positions
  (+ (cond [(= idx   c) -1] [(= idx c2) 1] [else 0])
     (cond [(= idx2 c2) -1] [(= idx2 c) 1] [else 0])))

(define (scramble! aboard row0 col0 scrambles)
  (for/fold ([row row0] [col col0])
            ([i (in-range scrambles)])
    (define j (random n-actions))
    (define drow (list-ref drows j))
    (define dcol (list-ref dcols j))
    (define row2 (+ row drow))
    (define col2 (+ col dcol))
    (cond [(and (< -1 row2 (board-n-rows aboard))
                (< -1 col2 (board-n-cols aboard)))
           (move aboard row col row2 col2 #:update!? #t)
           (values row2 col2)]
          [else (values row col)])))

;; Returns whether the stp problem (given as a list) is solvable, using a parity check.
;; Thanks to Levi Lelis, who thanks Neil Burch (he thinks)
(define (valid-stp-list? lst)
  (define size (length lst))
  (define positions (make-vector size 0))
  (for ([x (in-list lst)] [pos (in-naturals)])
    (vector-set! positions x pos))
  (define (position n) (vector-ref positions n))
  (define width (integer-sqrt size))
  (define t 0)
  (when (= (modulo width 2) 0)
    (set! t (quotient (position 0) width)))
  (for* ([i (range 2 size)]
         [j (range 1 i)])
    (when (< (position i) (position j))
      (set! t (+ t 1))))

  (= 0 (modulo t 2)))

;; Returns a truly random puzzle
(define (make-random-stp N)
  (define lst (shuffle (range (* N N))))
  ;; 50% chance to generate a valid instance
  (if (valid-stp-list? lst)
      lst
      (make-random-stp N)))

(define (stp-n-solved aboard)
  (for/sum ([x (in-bytes (board-vec aboard))] [i (in-naturals)])
    (if (= x i) 1 0)))

(define (stp->string stp)
  (table->string
   (for/list ([row (in-range (board-n-rows stp))])
     (for/list ([col (in-range (board-n-cols stp))])
       (define c (board-ref stp row col))
       (if (= c 0) " " (~a c #:width 3 #:align 'center))))))

;================;
;=== Contexts ===;
;================;

(define (get-contexts/setter stp nd set-next-context!)
  (define row0 (stp-state-row0 stp))
  (define col0 (stp-state-col0 stp))
  (define N² (* (board-n-rows stp) (board-n-cols stp)))
  (define bts (board-vec stp))

  (board-relative-tiling/setter stp
                                #:setter! set-next-context!
                                #:row row0 #:col col0 #:pad-value N² #:max-value N²
                                #:row-dist 3 #:col-dist 3 ; 5 possible positions for row
                                #:row-span 2 #:col-span 2) ; 4 cells of 25+1 possibilities
  (board-relative-tiling/setter stp
                                #:setter! set-next-context!
                                #:row row0 #:col col0 #:pad-value N² #:max-value N²
                                #:row-dist 2 #:col-dist 2 ; 3 possible positions for row
                                #:row-span 2 #:col-span 1) ; 2 cells of 25+1 possibilities
  (board-relative-tiling/setter stp
                                #:setter! set-next-context!
                                #:row row0 #:col col0 #:pad-value N² #:max-value N²
                                #:row-dist 2 #:col-dist 2
                                #:row-span 1 #:col-span 2)
  (board-relative-tiling/setter stp
                                #:setter! set-next-context!
                                #:row row0 #:col col0 #:pad-value N² #:max-value N²
                                #:row-dist 2 #:col-dist 2
                                #:row-span 1 #:col-span 1)
  (set-next-context!
   ;; No encoding necessary since there's only one number
   (bfs-node-last-action nd #:no-action n-actions)))

;==============;
;=== Search ===;
;==============;

(define (stp-state-copy stp)
  (stp-state (bytes-copy (board-vec stp))
             (board-n-rows       stp)
             (board-n-cols       stp)
             (stp-state-row0     stp)
             (stp-state-col0     stp)
             (stp-state-n-solved stp)))

(define (do-action stp act)
  (define row (stp-state-row0 stp))
  (define col (stp-state-col0 stp))
  (define drow (list-ref drows act))
  (define dcol (list-ref dcols act))
  (define row2 (+ row drow))
  (define col2 (+ col dcol))
  (cond [(board-in-bounds? stp row2 col2)
         ;; Copy only if needed
         (define stp2 (stp-state-copy stp))
         (define solved-diff (move stp2 row col row2 col2 #:update!? #t))
         (set-stp-state-n-solved! stp2 (+ (stp-state-n-solved stp) solved-diff))
         (set-stp-state-row0! stp2 row2)
         (set-stp-state-col0! stp2 col2)
         stp2]
        [else
         ; no change, no need to copy
         stp]))

(define (stp-solution? stp)
  (= (stp-state-n-solved stp)
     (bytes-length (board-vec stp))))

(define (get-visited-key stp _nd)
  ; no need to copy in bfs, since state is copied already
  (board->bytes stp))

(define solve-stp
  (make-bfs-solver #:n-actions n-actions
                   #:get-contexts/setter get-contexts/setter
                   #:get-visited-key get-visited-key
                   #:solution? stp-solution?
                   #:do-action do-action))

;=============;
;=== Mains ===;
;=============;

(module+ main
  (require lts-cm/worker)
  (domain-main #:spec->state list->stp #:solver solve-stp))

(module+ worker
  (require lts-cm/worker)
  (domain-worker #:spec->state list->stp #:solver solve-stp))

;================;
;=== DrRacket ===;
;================;

(module+ drracket
  (require racket/list
           racket/dict)
  (define stp1 (make-stp 5 #:n-scrambles 5))
  (print-board stp1)
  (define res (solve-stp stp1))
  (define res2 (filter-not (λ (p) (memq (car p) '(ctxs-seq act-seq))) res))
  (println res2)
  (define ctxs-seq (dict-ref res 'ctxs-seq))
  (define acts (dict-ref res 'act-seq))
  (unless (empty? ctxs-seq)
    (printf "#mutex_sets: ~a\n" (fxvector-length (first ctxs-seq)))
    (printf "actions: ~a\n" (apply string-append (map action->string acts)))))

;=============;
;=== Tests ===;
;=============;

;; Tests for validating `valid-stp-list?`:
(module+ test
  (require rackunit)
  (for ([i 100])
    (define lst (board->list (make-stp 4 #:n-scrambles (random 1000))))
    (check-pred valid-stp-list? lst)
    (define lst2 (board->list
                  (list->stp '(0 2 1 3 4 5 6 7 8 9 10 11 12 13 14 15)
                             #:n-scrambles (random 10000))))
    (check-false (valid-stp-list? lst2))))