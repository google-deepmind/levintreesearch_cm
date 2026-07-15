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
         global
         "flonum.rkt"
         define2)

(provide (all-defined-out))

(#%declare #:unsafe) ; Warning: Can be faster, but some bugs may become silent

;; ε-mix is to be used manually to mix a policy with the uniform distribution.
;; This is *not* done automatically and needs to be apply on a case-by-case basis.
;; ε-mix could be tuned online by looking at the maximum trajectory length,
;; such as with, at depth d, 1 - ε_d = log_3(d+3)/log_3(d+4), such that
;; the blowup factor at depth D is 1 / ∏_{d=0}^D (1-ε_d) = log_3(D+4)
(define-global *ε-mix* 1.e-3
  '("Mixing with the uniform distribution at prediction time."
    "Depends on the implementation.")
  (λ (x) (and (real? x) (inexact? x)))
  (λ (x) (define y (string->number x)) (and y (+ 0. y)))
  '("--eps-mix"))

;=================;
;=== Utilities ===;
;=================;

;; Returns the d/π cost for a given list l of conditional probabilities.
(define (pcond-seq->d/π pconds)
  (/ (length pconds)
     (apply * pconds)))

;=================================;
;== Contexts: Pattern DataBases ==;
;=================================;

;; Future-friendly product-mixing (softmax-like).
;; Returns a flvector of the conditional probabilities of the actions.
;; Some numerical stability, in particular for underflows:
;; θ'_i = exp(β_i - max_j β_j)
;; Z = ∑_i θ'_i
;; θ_i = θ'_i / Z .
;;
;; n-idx must be at most `(fxvector-length idxs)`, but can be used to use only a prefix set
;; of idxs
;;
;; idx : fxvector ; indices of the contexts to lookup in βmatrix
;; default-βs : (or #f flvector?)
;;   If provided, is used for unknown contexts.
(define (flproduct-mix/β-idx βmatrix idxs n-cols
                             #:vec-out [vec (make-flvector n-cols 0.)]
                             #:? [n-idxs (fxvector-length idxs)]
                             #:? [default-βs #f])
  (define fln-no-idx (fx->fl (fx- (fxvector-length idxs) n-idxs))) ; number of unknown contexts
  (define βmax
    (for/fold ([βmax -inf.0])
              ([i (in-range n-cols)])
      (define β
        (fl+
         (for/flsum ([idx (in-fxvector idxs)]
                     [_j (in-range (fx+ n-idxs))]) ; use only the first n-idxs elements
           (flvector-ref βmatrix (fx+ idx i)))
         (if default-βs
             (fl* fln-no-idx (flvector-ref default-βs i))
             0.)))
      (flvector-set! vec i β)
      (flmax (fl+ βmax) (fl+ β)))) ; the `fl+` is very important to avoid boxing flonum, which can be very costly!

  (define Z
    (for/flsum ([β (in-flvector vec)] [i (in-naturals)])
      (define θu (flexp (fl- β βmax)))
      (flvector-set! vec i θu)
      θu))

  (for ([θu (in-flvector vec)] [i (in-naturals)])
    (flvector-set! vec i (fl/ θu Z)))

  vec)

;; Like `flproduct-mix/β-idx`, but specialized for returning the probability of a single action.
(define (flproduct-mix-ref/β-idx βmatrix idxs n-cols act)
  (define θact 0.)
  (define βmax -inf.0)

  (for ([i (in-range (fx+ n-cols))])
    (define β
      (for/flsum ([idx (in-fxvector idxs)])
        (flvector-ref βmatrix (fx+ idx i))))
    (when (fl> β βmax)
      (set! βmax (fl+ β))))

  (define Z
    (for/flsum ([i (in-range (fx+ n-cols))])
      (define β
        (for/flsum ([idx (in-fxvector idxs)])
          (flvector-ref βmatrix (fx+ idx i))))
      (define θu (flexp (fl- β βmax)))
      (when (fx= i act)
        (set! θact (fl+ θu)))
      θu))

  (fl/ θact Z))

;:::::::::::::::::::::::::::::::::::::::::::::::::::;
;:: In-place product mixing + mixing with uniform ::;
;:::::::::::::::::::::::::::::::::::::::::::::::::::;

;;; Used in beta-matrix.rkt
(define (mix-uniform!/flvector pconds n-actions α)
  (define α/N (/ α n-actions 1.))
  (define 1-α (- 1. α))
  (for ([pc (in-flvector pconds)]
        [i (in-naturals)])
    (flvector-set! pconds i (fl+ α/N (fl* 1-α pc))))
  pconds)

