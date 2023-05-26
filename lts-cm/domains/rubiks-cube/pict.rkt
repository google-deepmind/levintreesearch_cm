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
         "rubiks-cube.rkt"
         define2)

(provide (all-defined-out))

;==================;
;=== pict utils ===;
;==================;

(define (hflip p)
  (inset (scale p -1. 1.)
         (pict-width p)
         0.))

(define (vflip p)
  (inset (scale p 1. -1.)
         0.
         (pict-height p)))

;=================;
;=== Cube pict ===;
;=================;

;; stickers: board of size 3x3 of stickers as seen when looking at the face
(define (make-face stickers #:? [size 10])
    (define colors #("white" "red" "blue" "orange" "green" "yellow"))
    (apply vc-append
           (for/list ([r 3])
             (apply hc-append
                    (for/list ([c 3])
                      (define color (vector-ref colors (board-ref stickers r c)))
                      (filled-rectangle size size #:color color #:border-width 1.))))))

;; On the left are the left, back and down faces seen through the cube (and not by rotating the cube)
;; On the right are the up, front, right faces.
(define (faces->pict faces)
  (define-values (up front right back left down) (apply values faces))

  (define front-pict (make-face front #:size 20))

  (define fur-pict
    (lb-superimpose
     (vl-append
      (scale (shear (make-face up #:size 20) -.5 0.)
             1. .5) ; up
      (blank (pict-width front-pict) (pict-height front-pict)))
     (hb-append
      front-pict
      (scale (shear (make-face right #:size 20) 0. -.5)
             .5 1.)))) ; right

  (define back-pict (hflip (make-face back #:size 20)))

  (define bdl-pict
    (lb-superimpose
     (rt-superimpose
      (vr-append
       (blank (pict-width back-pict) (pict-height back-pict))
       (scale (shear (vflip (make-face down #:size 20)) -.5 0.)
              1. .5)) ; down
      (ht-append
       (scale (shear (hflip (make-face left #:size 20)) 0. -.5)
              .5 1.) ; left
       back-pict))
     (vl-append (scale (shear (rectangle 60 60) -.5 0.)
                       1. .5)
                (rectangle 60 60))))

  (hc-append 1 bdl-pict fur-pict))

(define (cube->pict cub)
  (faces->pict (cube->face-boards cub)))

(module+ drracket
  (define cube1 (make-solved-cube))
  (cube->pict cube1)
  ;; Check all single rotations:
  (for ([act (in-range n-actions)])
    (println (cube->pict (do-action cube1 act)))))
