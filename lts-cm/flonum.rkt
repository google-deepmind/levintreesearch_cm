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

(require racket/flonum
         racket/performance-hint
         syntax/parse/define
         define2)

(#%declare #:unsafe) ; Warning: Can be faster, but some bugs may become silent

;; + maybe exports from `provide-safe-ops` below. (all of flonum + safe ops)
(provide (all-from-out racket/flonum)
         (all-defined-out))

(define-inline (flsqr x)
  (fl* x x))

(define (flvector-copy! dest dest-start src [src-start 0] [src-end (flvector-length src)])
  (for ([i (in-naturals src-start)]
        [j (in-range src-start src-end)])
    (flvector-set! dest i (flvector-ref src j))))

(define-inline (flvector-fill! vec val)
  (for ([i (in-range (flvector-length vec))])
    (flvector-set! vec i val)))

;; But take a look at flvector-sum in math/flonum (which is typed,
;; and the typed/untyped interface is too slow).
(define (flvector-sum v)
  (fl+ (for/flsum ([x (in-flvector v)]) x)))

(define (flvector-max v)
  (fl+ (for/fold ([m -inf.0]) ([x (in-flvector v)]) (flmax (fl+ m) (fl+ x)))))

;; Returns log ∑_i exp(v[i]) but more numerically stable, i.e.,
;; log[∑_i exp(v[i] - m)] + m,
;; where m = max_i v[i]
(define (flvector-log-sum-exp v)
  (define m (fl+ (flvector-max v)))
  (fl+
   m
   (fllog
    (for/flsum ([x (in-flvector v)])
      (flexp (fl- x m))))))

;; `expr` must produce a flonum
(define-syntax-parse-rule (for/flsum clauses body ... expr:expr)
  (for/fold ([s 0.])
            clauses
    body ...
    (fl+ s expr)))
#;(
;; Kahan's summation:
;; https://en.wikipedia.org/wiki/Kahan_summation_algorithm
;; Unfortunately, this slows down everything a lot :/
(define-syntax-parse-rule (for/flsum2 clauses body ... expr:expr)
  (for/fold ([sum 0.]
             [c 0.]
             #:result sum)
            clauses
    body ...
    (let* ([x (fl+ expr)]
           [y (fl- x c)]
           [t (fl+ sum y)]
           [d (fl- t sum)])
      (values (fl+ t) (fl- d y)))))

(module+ drracket
  (require math/flonum) ; TODO: Don't keep this here as it is required in the enclosing module

  (define l (build-list 10000 (λ _ (if (= 0 (random 2))
                                       (/ (flsqr (random)))
                                       (flsqr (random))))))
  (define (compare-sums l)
    (define s1 (flsum l))
    (define s2 (for/flsum ([x l]) x))
    (define s3 (for/flsum2 ([x l]) x))
    (list s1 s2 s3 (- s1 s2) (- s1 s3) (- s2 s3)))
  (compare-sums l)
  (compare-sums (sort l <))
  (compare-sums (sort l >))
  ))
