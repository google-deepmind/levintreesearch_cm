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

(require timev
         racket/fixnum
         racket/list
         "policy.rkt"
         "flonum.rkt"
         "misc.rkt"
         "cdb.rkt"
         define2)

(provide (all-defined-out))

#|
The βmatrix is just a long matrix (actually a vector) of all the flonum parameters.
Context codes (calculated in collect-contexts) are turned into indices in the βmatrix
using a hash (kept in the CDB).

This allows to have sparse context codes (still within fixnums) and a dense βmatrix.
In particular, when optimizing, context codes don't exist anymore, only indices to rows
in the βmatrix.

|#

;; act-seq: fxvector
;; idxs-seq: (vectorof fxvector)
;;   Indices within the βmatrix converted from context codes.
;; uniq-idxs: fxvector
;; log-loss: ℝ⁺
;; A trajectory corresponds to a solution. idxs-seq is like the ctxs-seq,
;; but with indices within the βmatrix instead of context codes.
;; All sequences are in 'forward' order: (first act-seq) is the first action taken in the trajectory.
(struct trajectory (length act-seq idxs-seq [log-loss #:mutable])
  #:transparent)

;; act-seq: fxvector/c
;; ctxs-seq: (vectorof fxvector/c)
;; ctx->idx: fixnum? -> fixnum?
;;
;; Notice: If ctx->idx has side effects (like hash-ref! for example),
;; then `make-trajectory` may have side effects.
(define (make-trajectory act-seq ctxs-seq mutex-dicts new-idx!)
  (define n-act-seq (fxvector-length act-seq))
  (define n-ctxs-seq (vector-length ctxs-seq))
  (assert (= n-act-seq n-ctxs-seq))
  (assert (> n-act-seq 0))
  (define n-mutex (vector-length mutex-dicts))

  (define idx-seq
    (for/vector #:length n-act-seq ([ctxs (in-vector ctxs-seq)])
      (define n-ctx (fxvector-length ctxs))
      (assert (= n-ctx n-mutex) ctxs)
      (for/fxvector #:length n-mutex
        ([ctx (in-fxvector ctxs)]
         [ctx.idx (in-vector mutex-dicts)])
        (hash-ref! ctx.idx ctx new-idx!))))

  (trajectory n-act-seq act-seq idx-seq 0.))

;; cdb: CDB?
;; act+ctxs-seqs: (vectorof (vector/c act-seq ctxs-seq))
;;   act-seq: fxvector/c
;;   ctxs-seq: (vectorof fxvector/c)
;; ε-low: nonnegative-real?
;; -> CDB? (listof trajectory?)
;; Returns a copy of `cdb` extended with context indices gathered from act+ctxs-seqs,
;; and the corresponding trajectories to be used for optimization.
(define (extend-cdb+trajectories cdb act+ctxs-seqs #:! ε-low)
  (define n-cols      (CDB-n-cols cdb))
  (define n-rows-init (CDB-n-rows cdb))
  (define n-traj      (vector-length act+ctxs-seqs))

  ;; Number of mutex sets
  (define n-mutex
    (if (CDB-mutex-dicts cdb)
        ;; If the old cdb already has a vector of ctx.idx, use it
        (vector-length (CDB-mutex-dicts cdb))
        ;; otherwise, try to guess the number from the trajectories
        (and (> n-traj 0)
             ;; get the number of contexts from the first action step in the first
             ;; trajectory that with at least one action
             (for/first ([act+ctxs-seq (in-vector act+ctxs-seqs)]
                         #:do [(define ctxs-seq (vector-ref act+ctxs-seq 1))]
                         #:unless (= 0 (vector-length ctxs-seq)))
               (fxvector-length (vector-ref ctxs-seq 0))))))

  (define mutex-dicts
    ;; Create one hasheq per mutex set
    (if (CDB-mutex-dicts cdb)
        ;; Copy the ctx->idx mappings from the old cdb
        (for/vector ([ctx.idx (in-vector (CDB-mutex-dicts cdb))])
          (hash-copy ctx.idx))
        ;; Create fresh mappings
        (and n-mutex
             (build-vector n-mutex (λ _ (make-hasheq))))))

  ;; The number of rows will increase every time a new context code
  ;; is observed, and the context code is mapped to a new row index.
  (define n-rows n-rows-init)
  (define (new-idx!)
    (++ n-rows)
    (* n-cols (- n-rows 1)))

  ;; Collect contexts from the trajectories
  ;; Notice: futurizing this is a little tricky because `hash-ref!` is *not*
  ;; fully atomic.
  (define trajectories
    (timev
     "make-trajectories"
     (for/list ([act+ctxs-seq (in-vector act+ctxs-seqs)]
                #:do [(define acts (vector-ref act+ctxs-seq 0))]
                #:unless (= 0 (fxvector-length acts)))
       (make-trajectory acts (vector-ref act+ctxs-seq 1) mutex-dicts new-idx!))))

  ;; When optimizing on the β-simplex, the sum of the βs tends to (A-1)ln ε, so
  ;; we'd better start at β_{c, a} = (A-1)/A × ln ε directly.
  ;; When optimizing on the hypercube, it doesn't really matter much, so we can
  ;; use the same values.
  (define β0 (fl* (fl- 1. (fl/ (fx->fl n-cols)))
                  (fllog ε-low)))
  (define βmatrix (make-flvector (fx* n-rows n-cols) β0))

  ;; Copy the previous βmatrix in the new one. Common context indices are the same.
  (flvector-copy! βmatrix 0 (CDB-βmatrix cdb))

  (define new-cdb (CDB mutex-dicts n-rows n-cols βmatrix))

  (values new-cdb trajectories))

(define (make-cdb-predictor cdb)
  (define n-actions     (CDB-n-cols      cdb))
  (define mutex-dicts   (CDB-mutex-dicts cdb)) ; ctx code to index in βmatrix converter
  (define βmatrix       (CDB-βmatrix     cdb))
  (define uniform-flvec (make-flvector n-actions (fl/ (fx->fl n-actions))))
  (define vec-out       (make-flvector n-actions 0.))
  (define idxs          #f)

  (if (not mutex-dicts)
      (λ (ctxs ε-mix #:? [default-βs #f]) uniform-flvec)
      (λ (ctxs ε-mix #:? default-βs)
        (unless idxs
          ;; Create the vector of indices in case it wasn't created before.
          ;; This assumes that the number of mutex sets is constant.
          (set! idxs (make-fxvector (fxvector-length ctxs))))

        ;; Turn the vector of contexts codes into a vector of indices idx
        ;; within the βmatrix. Some codes don't have entries (yet) into
        ;; the beta matrix, so we just assume the corresponding prob. dist.
        ;; is the uniform distribution, which does not play any role in the
        ;; computation and can thus be omitted.
        ;; n-idxs is passed to  `flproduct-mix/β-idx`, which
        ;; allows to use a fixed fxvector, but of (bounded) variable length.
        ;; If the context does not map to an index, *no* new entry in the
        ;; βmatrix is created.
        (define n-idxs 0)
        (for ([ctx (in-fxvector ctxs)]
              [ctx.idx (in-vector mutex-dicts)])
          (define idx (hash-ref ctx.idx ctx #f))
          (when idx
            (fxvector-set! idxs n-idxs idx)
            (++ n-idxs)))

        (cond [(= 0 n-idxs)
               uniform-flvec]
              [else
               (flproduct-mix/β-idx βmatrix idxs n-actions
                                    #:vec-out vec-out #:n-idxs n-idxs #:default-βs default-βs)
               (mix-uniform!/flvector vec-out n-actions ε-mix)
               vec-out]))))
