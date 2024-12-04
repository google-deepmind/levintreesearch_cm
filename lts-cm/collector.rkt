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
         "encode.rkt"
         "misc.rkt"
         define2)

(provide (all-defined-out))

#|
Contexts are encoded into fixnums (usually 2^60-1 values) to be used as
keys to a hasheq for efficiency.

The number of mutex set is assumed to be constant, but does not need
to be specified by the domain's `collect-contexts`.

This means that when designing the context encodings, one must
make sure that the codes fit within a fixnum.

For example, for a relative tiling of row-span 3 and col-span 2
with max-value 5, the maximum code is
(5+1)^{3×2} << (most-positive-fixnum)
but with row-span 5 and col-span 5
(5+1)^{5×5} > (most-positive-fixnum)
so a tiling this big is not possible.
  Side note: At the same time, this would lead
    to far too many contexts and parameters,
    or the coding is very sparse and could be made more dense.
|#

;; A simple collector that returns the list of contexts.
(define (make-list-collector)
  (define l '())
  (case-lambda
    [() (reverse l)]
    [(x) (set! l (cons x l))]))

;; Returns a procedure `fxvector-encode!` that, once called, encodes the given number within the
;; vector at the current index, along with its index in the vector, and moves the current index.
;; `fxvector-encode!` must not be called more times than the length of the vector.
;; Meant to be used with `collect-contexts`.
(define (make-fxvector-collector vec)
  (define i 0)
  (λ (v)
    (fxvector-set! vec i v)
    (++ i)))

;; Returns a procedure to be used with context collectors (with one argument),
;; and can be called afterwards with 0 arguments to retreive the fxvector of contexts.
;; Slower than `make-fxvector-collector` as it allocates a vector and a list.
;; Can be used for debugging `collect-contexts`.
(define (make-fxvector-collector/auto)
  (define ctxs '())
  ;; We also encode the position in the vector, that is, the mutex set index.
  (case-lambda
    [(v) (set! ctxs (cons v ctxs))]
    [() (apply fxvector (reverse ctxs))]))
