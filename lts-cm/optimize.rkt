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
         racket/format
         racket/fixnum
         racket/future
         racket/list
         racket/match
         racket/math
         global
         safe-case
         text-table
         text-table/utils
         timev
         "beta-matrix.rkt"
         "cdb.rkt"
         "date.rkt"
         "delta-secant.rkt"
         "flonum.rkt"
         "future.rkt"
         "log-file.rkt"
         "misc.rkt"
         "policy.rkt"
         "verbose.rkt"
         define2)

(provide (all-defined-out))

(#%declare #:unsafe) ; Warning: Can be faster, but some bugs may become silent

;***************************************************************************************;
;****                        Optimization For Context Models                        ****;
;***************************************************************************************;

(define-global:real *ε-low* 1.e-4 "lower bound on the β parameters")

(define-global *gap-factor* .5
  '("Optimization stops when the duality gap is less than a factor gap-factor of the current loss."
    "After this, if the gap-factor is α then provably the loss is less than the optimum loss"
    "times 1/(1-α).")
  (λ (x) (and (number? x) (< 0 x 1)))
  string->number)

(define-global:natural1 *n-futures* (exact-ceiling (/ (processor-count) 2))
  "Number of futures to use during optimization")

;====================;
;=== Trajectories ===;
;====================;

;; trajectory: (vector/c actions contexts-sequence)
;;   actions: fxvector
;;   contexts-sequence: (vectorof contexts)
;;   contexts: fxvector

;; cdb               : CDB? ; context database
;; βmatrix           : flvector ; matrix of the parameters as a long flvector
;; jac               : flvector ; matrix as a long flvector, same dimensions as βvecs
;; future-grads      : (vectorof flvector) ; copies of the gradient for parallelization
;; Δvec              : flvector? ; inverse learning rates (kind of) per context
;; trajectory-groups : (vectorof (vectorof trajectory?))
;; idx-groups        : (vectorof fxvector)
;; ε-low             : flonum ; lower bound on the values in βmatrix
;; β0                : flonum ; initial value of the parameters, also used in regularizer
;; max-log-loss      : flonum ; maximum log loss of an individual trajectory.
;;                              Used for numerical stability.
(struct optimization (cdb
                      jac
                      future-grads
                      Δvec
                      trajectory-groups
                      idx-groups
                      ε-low
                      β0
                      [max-log-loss #:mutable])
  #:transparent)

(define (optimization-n-cols  optim) (CDB-n-cols  (optimization-cdb optim)))
(define (optimization-n-rows  optim) (CDB-n-rows  (optimization-cdb optim)))
(define (optimization-βmatrix optim) (CDB-βmatrix (optimization-cdb optim)))

;; Notice: idx is the idx of βmatrix and jac
(define-syntax-rule (Δ-ref optim idx)
  (flvector-ref (optimization-Δvec optim)
                (fxquotient (fx+ idx) (fx+ (optimization-n-cols optim)))))

;; Notice: idx is the idx of βmatrix and jac
(define (Δ-set! optim idx v)
  (flvector-set! (optimization-Δvec optim)
                 (fxquotient (fx+ idx) (fx+ (optimization-n-cols optim)))
                 v))

;:::::::::::::::::::::::::::;
;:: d/π cost in log space ::;
;:::::::::::::::::::::::::::;

(define (trajectory-logd/π optim traj βmatrix vec-out)
  (define n-acts (fx+ (optimization-n-cols optim)))
  (fl-
   (fllog (fx->fl (trajectory-length traj)))
   (for/flsum ([idxs (in-vector   (trajectory-idxs-seq traj))]
               [act  (in-fxvector (trajectory-act-seq traj))])
     (fllog (fl+ (flproduct-mix-ref/β-idx βmatrix idxs n-acts act))))))

;; Returns the sum of the losses and the maximum log loss
(define (trajectories-logd/π/futures optim #:? [βmatrix (optimization-βmatrix optim)] #:? [save? #f])
  (define groups (optimization-trajectory-groups optim))
  (define n-groups (vector-length groups))
  (define n-cols (optimization-n-cols optim))
  (define sums  (make-flvector n-groups 0.))
  (define maxes (and save? (make-flvector n-groups -inf.0)))
  (define vecs
    ;; This is taking next to 0 time
    (for/vector #:length n-groups ([i (in-range n-groups)])
      (make-flvector n-cols 0.)))
  (define loss-vecs
    ;; Also taskes 0 ms
    (for/vector #:length n-groups ([trajs (in-vector groups)])
      (make-flvector (vector-length trajs))))
  (for/async ([trajs (in-vector groups)]
              [i (in-naturals)]
              [vec-out (in-vector vecs)]
              [loss-vec (in-vector loss-vecs)])
    (for ([traj (in-vector trajs)] [j (in-naturals)])
      (define logd/π (fl+ (trajectory-logd/π optim traj βmatrix vec-out)))
      (when save? (set-trajectory-log-loss! traj (fl+ logd/π)))
      (flvector-set! loss-vec j (fl+ logd/π)))
    (when save? (flvector-set! maxes i (fl+ (flvector-max loss-vec))))
    (flvector-set! sums i (fl+ (flvector-log-sum-exp loss-vec))))

  (when save? (set-optimization-max-log-loss! optim (fl+ (flvector-max maxes))))

  (fl+ (flvector-log-sum-exp sums)))

;; Returns the log ∑ d/π costs.
;; NOTICE: Requires that `trajectories-logd/π/futures` has been called beforehand.
(define (trajectories-logd/π-saved optim)
  (define max-log-loss (optimization-max-log-loss optim))

  ;; custom log-sum-exp because we know max-log-loss already and we can avoid
  ;; allocating the vector. No future though.
  ;; We could have a for/log-sum-exp maybe, possibly with #:offset option like #:length option
  ;; for for/vector.
  (fl+
   max-log-loss
   (fllog
    (for*/fold ([sum 0.])
               ([trajs (in-vector (optimization-trajectory-groups optim))]
                [traj (in-vector trajs)])
      (fl+ sum (flexp (fl- (trajectory-log-loss traj)
                           max-log-loss)))))))

;; Returns the list of the d/π losses of all trajectories (useful for statistics)
;; NOTICE: Requires that `trajectories-logd/π/futures` has been called beforehand.
(define (trajectories-losses-saved optim)
  (for*/list ([trajs (in-vector (optimization-trajectory-groups optim))]
              [traj (in-vector trajs)])
    (flexp (trajectory-log-loss traj))))


;; ----------

(define (optimization-n-trajectories optim)
  (for/sum ([group (in-vector (optimization-trajectory-groups optim))])
    (vector-length group)))

(define (trajectory-lengths optim)
  (for*/list ([trajs (in-vector (optimization-trajectory-groups optim))]
              [traj (in-vector trajs)])
    (trajectory-length traj)))

(define (trajectories-length optim)
  (for*/sum ([trajs (in-vector (optimization-trajectory-groups optim))]
             [traj (in-vector trajs)])
    (trajectory-length traj)))

;; Prints some statistics about the optimization.
;; Recalculates the gradient, so may take time.
(define (print-optimization-stats optim #:? [reg 0.])
  ;; Recalculate gradient to make sure the statistics are correct
  (timev "jac" (gradient! optim))
  (gradient-add-regularizer! optim reg)

  (timev
   "optimization-stats"
   (print-date-iso)
   (define costs  (timev "traj-costs" (trajectories-losses-saved optim)))
   (timev
    "print-list-stats"
    (print-list-stats
     (cons "Cost (no reg, ε=0)"   costs)
     (cons "Length" (trajectory-lengths optim))
     (cons "dual-gap/βsimplex" (duality-gap/βsimplex optim))))
   (define tot-cost (apply + costs))
   (define reg-cost (if reg (regularization-cost optim reg) 0.))
   (define cost+reg (+ tot-cost reg-cost))
   (print-simple-table
    #:->string (list ~a (~r* #:precision '(= 2)))
    #:align '(left right)
    (list (list "cost" tot-cost)
          (list "reg-cost" reg-cost)
          (list "cost+reg" cost+reg)))
   (printf "#contexts: ~a\tε-low: ~a\tε-mix: ~a\n"
           (optimization-n-rows optim)
           (optimization-ε-low optim)
           (*ε-mix*))
   cost+reg))

(define (duality-gap/βsimplex optim)
  (define ε       (optimization-ε-low   optim))
  (define jac     (optimization-jac     optim))
  (define βmatrix (optimization-βmatrix optim))
  (define n-cols  (optimization-n-cols  optim))
  (define n-rows  (optimization-n-rows  optim))
  ;; Since the gradients are divided by the max loss, we need to invert this here
  (define eC (flexp (optimization-max-log-loss optim)))
  (define lnε (fllog ε))
  ;; could be future'd
  (for/list ([row (in-range n-rows)])
    (define imin
      (for/fold ([gmin +inf.0] [imin -1] #:result imin)
                ([g (in-flvector jac     (fx* row n-cols) (fx* (fx+ 1 row) n-cols))]
                 [i (in-naturals)])
        (if (< g gmin)
            (values g i)
            (values gmin imin))))
    (for/sum ([g (in-flvector jac     (fx* row n-cols) (fx* (fx+ 1 row) n-cols))]
              [β (in-flvector βmatrix (fx* row n-cols))]
              [i (in-naturals)])
      (if (fx= i imin)
          (fl* eC g β) ; = g×(β-0), recall β ≤ 0
          (fl* eC g (fl- β lnε))))))


;=====================================;
;=== Optimizing d/π surrogate loss ===;
;=====================================;

(define (result->act+ctxs-seq res)
  (and (eq? (dict-ref res 'status) 'SOLVED)
       (vector (apply fxvector (dict-ref res 'act-seq))
               (apply vector (dict-ref res 'ctxs-seq)))))

;; Can be used directly with the results of the search algorithms.
;; results: (listof result?) ; a result is a dictionary as returned by `bfs`.
;; old-cdb: CDB? ; Used to initialize the weights of the new cdb. old-cdb is not modified.
;;   An empty cdb can be created with `(make-cdb n-actions)`.
(define (results->optimization results #:! old-cdb #:? ε-low)
  (define act+ctxs-seqs (apply vector (filter-map result->act+ctxs-seq results)))
  (act+ctxs-seqs->optimization act+ctxs-seqs #:old-cdb old-cdb #:ε-low ε-low))

;; act+ctxs-seqs: (vectorof (vector/c act-seq ctxs-seq))
;;   act-seq: fxvector/c
;;   ctxs-seq: (vectorof fxvector/c)
;; old-cdb: CDB? ; Used to initialize the weights of the new cdb. old-cdb is not modified.
;; ε:smallest value that a single context can take
;; n-cols = n-actions
(define (act+ctxs-seqs->optimization act+ctxs-seqs #:! old-cdb #:? [ε-low (*ε-low*)])

  (define-values (cdb trajectories)
    (extend-cdb+trajectories old-cdb act+ctxs-seqs #:ε-low ε-low))

  (define n-rows  (CDB-n-rows  cdb))
  (define n-cols  (CDB-n-cols  cdb))
  (define βmatrix (CDB-βmatrix cdb))
  (define jac    (make-flvector (fx* n-rows n-cols) 0.))
  (define Δvec   (make-flvector n-rows 0.))

  (define n-proc (*n-futures*))
  (define future-grads (timev "copy grads" (build-vector n-proc (λ _ (flvector-copy jac)))))
  ;; Group trajectories to spread among futures
  ;; This allows to keep exactly n-proc futures running without incurring the cost of
  ;; starting/stopping len futures instead
  (define len (length trajectories))
  (define trajectory-groups
    ;; List of vectors
    (timev "trajectory-groups" (list->subvectors trajectories n-proc)))
  ;; Group gradient rows to spread among futures.
  ;; Not sure if fxvector is better than list here.
  (define idx-groups
     ;; indices of the rows in the matrix, grouped.
    (timev "idx-groups"
           (list->subvectors (range 0 (fx* n-rows n-cols) n-cols) n-proc #:vector-kind 'fx)))

  ;; Center of the "β-simplex"
  (define β0 (fl* (fl- 1. (fl/ (fx->fl n-cols))) (fllog ε-low)))

  (optimization cdb jac future-grads Δvec trajectory-groups idx-groups ε-low β0 0.))

(define (reset-Δvec! optim)
  (flvector-fill! (optimization-Δvec optim) 0.))

;; NOTICE: Assumes that trajectory-logd/π has been called just before, with `#:save? #t`.
(define (gradient!/trajectory/logd/π optim traj #:? [jac (optimization-jac optim)] #:? βvec-out)
  (unless (= 0 (trajectory-length traj))
    (define βmatrix       (optimization-βmatrix  optim))
    (define n-cols   (fx+ (optimization-n-cols   optim))) ; fx+: compiler hint

    (define logd/π (fl+ (trajectory-log-loss traj)))
    (define C (fl+ (optimization-max-log-loss optim)))
    (define L (flexp (fl- logd/π C)))

    (for ([act        (in-fxvector (trajectory-act-seq  traj))]
          [idxs       (in-vector   (trajectory-idxs-seq traj))]
          [i          (in-naturals)])
      ;; If there are several contexts, we use product mixing.
      ;; Note: A product mixing with one context is just like the special case above,
      ;;   except for a constant offset which goes away with the Prod update.
      (define pxs (flproduct-mix/β-idx βmatrix idxs n-cols #:vec-out βvec-out))
      (for ([act2  (in-naturals)]
            [px    (in-flvector pxs)])
        (define g (fl+ (if (fx= act act2)
                           (fl* L (fl- px 1.)) ; chosen action
                           (fl* L px)))) ; other action
        (for ([idx (in-fxvector idxs)])
          (define idx2 (fx+ idx act2))
          (define old-g (flvector-ref jac idx2))
          (flvector-set! jac idx2 (fl+ old-g g)))))))

(define (reset-gradient! optim)
  (define jac (optimization-jac optim))
  (define idx-groups (optimization-idx-groups optim))
  ;; these could be memoized
  (define starts (for/list ([idxs (in-vector idx-groups)])
                   (fxvector-ref idxs 0)))
  (define ends (append (cdr starts) (list (flvector-length jac))))

  ;; Not sure parallelizing this is really faster. It should be easy to test standalone though
  (for/async ([start (in-list starts)] [end (in-list ends)])
    (for ([i (in-range start end)])
      (flvector-set! jac i 0.))))


;; Calculates the gradient but also sets the min-max-loss
(define (gradient! optim #:? [futures? #t])
  (timev "reset-gradient" (reset-gradient! optim))
  (cond
    [futures?
     ;; 1. Calculate the trajectories log-costs and store them in each trajectory
     ;; 2. Get the max-log-cost and for each trajectory calculate
     ;;   exp(log-cost(β) - max-log-cost) ∇ log-cost(β)
     ;;
     ;; When using Adagrad on top of this (no reset?)
     ;; this may actually be normalized GD with the infinite norm.
     (timev "trajectories-logd/π/futures+save" (trajectories-logd/π/futures optim #:save? #t))

     ;; With futures. Can be significantly faster than serial.
     (define grads (optimization-future-grads optim))
     (for/async ([trajs (in-vector (optimization-trajectory-groups optim))]
                 [jac (in-vector grads)])
       (flvector-fill! jac 0.) ; We need to reset *all* grads
       (define βvec-out (make-flvector (optimization-n-cols optim) 0.)) ; one per future
       (for ([traj (in-vector trajs)])
         (gradient!/trajectory/logd/π optim traj #:jac jac #:βvec-out βvec-out)))
     ;; Notice: This can take a lot of time when there are few trajectories
     (timev "sum-async-grads" (flvectors-sum/async grads #:vec-out (optimization-jac optim)))]
    [else
     ;; Serial.
     (for* ([trajs (in-vector (optimization-trajectory-groups optim))]
            [traj (in-vector trajs)])
       (gradient!/trajectory/logd/π optim traj))]))


(define (gradient-add-regularizer! optim reg)
  (define jac     (optimization-jac     optim))
  (define βmatrix (optimization-βmatrix optim))
  (define n-cols  (optimization-n-cols  optim))
  (define β0      (optimization-β0      optim))
  ;; Because the losses are divided by the max loss, we also need to scale the regularization
  ;; similarly.
  (define C (optimization-max-log-loss optim))
  (define R (fl* reg (flexp (fl- C))))

  (for/async ([idxs (in-vector (optimization-idx-groups optim))])
    (for ([idx (in-fxvector idxs)])
      (for ([i (in-range idx (+ idx n-cols))] [g (in-flvector jac idx)] [β (in-flvector βmatrix idx)])
        ;; Quadratic regularizer ½||β - β0||²
        ;; We choose β0 to be the 'center' of the β-simplex,
        ;; so as to push weights towards the uniform distribution.
        ;; This regularizer can be used with FW on both the β-simplex and the β-hypercube,
        ;; as well as GD on the hypercube.
        (flvector-set! jac i (fl+ g (fl* R (fl- β β0))))))))

;; The regularizer's cost, on the same scale as losses.
(define (regularization-cost optim reg #:? [βmatrix (optimization-βmatrix optim)])
  (define n-cols  (optimization-n-cols  optim))
  (define β0      (optimization-β0      optim))
  (define reg2 (fl* .5 (fl+ reg))) ; compiler hint

  (define vec (make-flvector (vector-length (optimization-idx-groups optim)) 0.))
  (for/async ([idxs (in-vector (optimization-idx-groups optim))]
              [ivec (in-naturals)])
    (define s
      (for/flsum ([idx (in-fxvector idxs)])
        (for/flsum ([i (in-range n-cols)] [β (in-flvector βmatrix idx)])
          (fl* reg2 (flsqr (fl- β β0))))))
    (flvector-set! vec ivec s))
  (flvector-sum vec))

;=====================================================;
;=== Update methods for line search (but not only) ===;
;=====================================================;

;; A little DSL to catch symbol mistakes
(define-safe-case case-stage (init line-search final))

#|
This is a variant of the isoGD algorithm described in the paper:
  Isotuning With Applications To Scale-Free Online Learning
  https://arxiv.org/abs/2112.14586

Differences:
* One learning rate per context (rather than per parameter, or one globally)
  Δvec is the vector of learning rates.
* We are in the batch optimization case, so we avoid the off-by-one issue
  by looking at the gradient norm before updating.
* The learning rates can be resetted (on square steps, or powers of 2 steps),
  so as to forget about previous large(r) gradients.
* The learning rate is still multiplied by a factor found by line search.

The learning rates 'tune' the line search direction,
which becomes different from the gradient direction
while the line search finds the strength of the update.
|#
(define (update/iso-gradient-descent! optim t
                                      #:βmatrix-out [βout (optimization-βmatrix optim)]
                                      #:! α
                                      #:! stage
                                      #:? [resets? #t])
  (define jac     (optimization-jac     optim))
  (define βmatrix (optimization-βmatrix optim))
  (define n-cols  (optimization-n-cols  optim))
  (define ε       (optimization-ε-low   optim))
  (define lnε (fllog ε))

  (case-stage
   stage
   ;; Before the line search
   [(init)
    ;; Reset Δvec on t=(1, 4, 9, 16, 25, …) in case a context has suffered
    ;; much larger losses than currently. The line search guarantees improvement
    ;; so resetting every so often won't be harmful and can speed up the update
    ;; greatly.
    (when (and resets? (even? t) (= t (sqr (exact-floor (sqrt t)))))
      (when-verb (displayln "*** isoGD RESET Δvec ***"))
      (reset-Δvec! optim))
    ;; Initialize Δvec with the current gradients.
    (for/async ([idxs (in-vector (optimization-idx-groups optim))])
      (for ([idx (in-fxvector idxs)])
        (define G² (for/flsum ([g (in-flvector jac idx (fx+ idx n-cols))])
                     (flsqr g)))
        (define G (flsqrt G²))
        (define Δ (Δ-ref optim idx))
        ; fl>= ensures that if G=Δ=0, it doesn't fail
        (Δ-set! optim idx (fl+ Δ (if (fl>= G Δ) G (fl/ G² Δ))))))]
   ;; During the line search
   [(line-search)
    ;; No projection to ensure the line search is convex.
    ;; No update of Δ
    (for/async ([idxs (in-vector (optimization-idx-groups optim))])
      (for ([idx (in-fxvector idxs)])
        (for ([i (in-range idx (fx+ idx n-cols))]
              [g (in-flvector jac idx)]
              [β (in-flvector βmatrix idx)])
          (define Δ (Δ-ref optim idx))
          (define η (if (fl= 0. Δ) 0. (fl/ Δ)))
          (flvector-set! βout i (fl- β (fl* α η g))))))]
   ;; After the line search
   [(final)
    (for/async ([idxs (in-vector (optimization-idx-groups optim))])
      (for ([idx (in-fxvector idxs)])
        (define Δ (Δ-ref optim idx))
        (define η (if (fl= 0. Δ) 0. (fl/ Δ)))
        (for ([i (in-range idx (fx+ idx n-cols))]
              [g (in-flvector jac idx)]
              [β (in-flvector βmatrix idx)])
          (flvector-set! βout i (flmax lnε (flmin 0. (fl- β (fl* α η g))))))))]))

;===================;
;=== Line search ===;
;===================;

(define (batch/line-search optim #:! step #:? [α-guess .5] #:! reg)

  (timev "grad" (gradient! optim))
  (timev "reg" (gradient-add-regularizer! optim reg))

  (define βmatrix      (optimization-βmatrix optim))
  (define jac          (optimization-jac     optim))
  (define n-cols  (fx+ (optimization-n-cols  optim))) ; fx+ to avoid boxing

  ;; α is in [0, 1]
  ;; Stage ∈ '(init line-search final)
  (define (update-βmatrix! mat α stage)
    ;; No need to project since we only consider points inside the domain
    (update/iso-gradient-descent! optim step #:βmatrix-out mat #:α α #:stage stage))

  (define β2matrix (flvector-copy βmatrix))
  (update-βmatrix! β2matrix 0. 'init)

  (define (query α)
    (timev
     "query"
     (update-βmatrix! β2matrix α 'line-search)
     (define log-cost
       (if (zero? α)
           (timev "trajs" (trajectories-logd/π-saved optim))
           (timev "trajs" (trajectories-logd/π/futures optim #:βmatrix β2matrix))))
     (define reg-cost (timev "reg" (regularization-cost optim reg #:βmatrix β2matrix)))
     ;; Notice: With a quadratic regularizer, this may not be convex, but should
     ;; still be quasi-convex, and should still be convex near the optimum.
     ;; The non-convexity *might* make the quasi-exact line search fail (this
     ;; has never happened so far).
     ;; An exponential regularizer should be investigated instead.
     (flvector-log-sum-exp (flvector log-cost (fllog reg-cost)))))

  ;; Line search
  (define query-0 (query 0.))
  (define η1 (if (< 0. α-guess 1.) α-guess 0.5)) ; protect against boundaries
  (define query-η1 (query η1))
  (define do-line-search?
    (or (<= 1 (modulo step 20) 3)
        (>= query-η1 query-0))) ; or no improvement with current learning rate

  (define-values (η fη)
    (cond [(not do-line-search?)
           ;; Try to go fast by re-using the same learning rate as last time.
           (values η1 query-η1)]
          ;; Or be more careful by performing a line search to find a good learning rate.
          [else
           (define line-dic
             (quasi-exact-line-search
              query 0. (* 2. η1)
              #:yleft query-0
              #:xq η1 #:yq query-η1
              ;; not using the gradient information for now
              #:callback
              (λ (dic)
                (when-verb
                 (define-assoc (iter pts x- x+ xgap ya yb ygap)
                   dic)
                 (match-define (list (pt x0 y0) (pt x1 y1) (pt x2 y2) (pt x3 y3) (pt x4 y4))
                   pts)
                 (print-simple-table
                  #:->string (list ~a (~r* #:precision '(= 6)))
                  (list '(    iter x1 x2 x3 y1 y2 y3 ygap xgap)
                        (list iter x1 x2 x3 y1 y2 y3 ygap xgap)))))))
           (values (dict-ref line-dic 'xlow) (dict-ref line-dic 'ylow))]))

  ;; NOTICE: The cost is before projection, which means it can be a little different after projection!
  ;; Thus we can't really pass the cost down for the next iteration, as we still need to calculate the
  ;; cost after projection
  (when-verb (displayln (list 'η: η 'fη: fη)))
  (update-βmatrix! βmatrix η #;stage: 'final)

  (values η fη))


;====================;
;=== Update loops ===;
;====================;

(define (print-date-iso)
  (displayln (string-append "Time is " (date-iso-file))))

;; Runs the update loop for the given βmatrix within optim (mutates it).
;; Also depends on `*gap-factor*`
(define (update-loop optim
                     #:! reg
                     #:! Tmax
                     #:? [Tprint 20]
                     #:? [register (λ (k v) (void))])
  (define tot-length (trajectories-length optim))
  (define n-trajs (optimization-n-trajectories optim))
  (let loop ([t 1] [α-guess 0.5])
    (when-verb
      (newline)
      (writeln (ids->assoc t α-guess)))
    (define-values (new-α _cost)
      (timev "update" (batch/line-search optim #:step t #:α-guess α-guess #:reg reg)))

    (cond [(= 0 (modulo t Tprint)) ; Below is costly so we amortize
           ;; Need to update the gradient to calculate the stats. Might be a little wasteful.
           (gradient! optim)
           (gradient-add-regularizer! optim reg)
           (define cost (flexp (trajectories-logd/π-saved optim)))
           (define reg-cost (regularization-cost optim reg))
           (define tot-cost (+ 0. cost reg-cost))
           (define gap (apply fl+ (duality-gap/βsimplex optim)))
           ;; This is not the actual gap factor, only a ratio
           (define dual-gap-factor (/ gap tot-cost))
           ;; This is the actual gap factor, but it is always +inf.0 unless gap < tot-cost.
           ;; We have a guarantee that the loss is less than dual-gap-ratio * optimal-loss.
           (define dual-gap-ratio (if (>= gap tot-cost) +inf.0 (/ (- 1. (/ gap tot-cost)))))
           (define cost/length (/ tot-cost tot-length))
           (define cost-no-reg/length (/ cost tot-length))
           (define avg-cost (/ tot-cost n-trajs))
           (define avg-cost-no-reg (/ cost n-trajs))
           (print-ids-table [t α-guess cost reg-cost tot-cost gap dual-gap-factor dual-gap-ratio
                               cost/length cost-no-reg/length avg-cost avg-cost-no-reg]
                            #:->string (list ~a
                                             (~r* #:precision '(= 8))
                                             (~r* #:precision '(= 2))))
           (cond
             [(> dual-gap-factor (*gap-factor*))
              (loop (+ t 1) new-α)]
             [else
              (register 'optim-steps t)])]
          [(< t Tmax)
           (loop (+ t 1) new-α)]
          [else
           (register 'optim-steps t)])))

;; register: sym value -> any ; may have side effects
;; This is the update function called by the server.
;; It loads the cdb file in log-dir (or creates a new one), as well as all the fasl files
;; generated during search, creates an `optimization` object,
;; runs the update loop and finally saves the updated cdb into the file.
(define (update-for-server #:! log-dir
                           #:! reg
                           #:! Tmax
                           #:? Tprint
                           #:! n-actions
                           #:! register)
  (define cdb-file (build-path log-dir cdb-file-name))
  (define cdb (cond [(file-exists? cdb-file)
                     (timev "load-cdb" (load-cdb cdb-file))]
                    [else (displayln "CDB file does not exist. Creating empty cdb.")
                          (make-cdb n-actions)]))

  (print-date-iso)
  (define act+ctxs-seqs (timev "log-dir->act+ctxs-seq" (log-dir->act+ctxs-seqs log-dir)))
  (define optim
    (timev "act+ctxs-seqs->optim" (act+ctxs-seqs->optimization act+ctxs-seqs #:old-cdb cdb)))
  (register 'n-contexts (optimization-n-rows optim))
  (register 'n-trajectories (optimization-n-trajectories optim))
  (when-verb (print-optimization-stats optim #:reg reg))

  (update-loop optim #:reg reg #:Tprint Tprint #:register register #:Tmax Tmax)

  (timev "save-cdb" (save-cdb (optimization-cdb optim) cdb-file))
  (define cost-stats
    (list-stats-table #:header? #f
                      (cons "Cost" (trajectories-losses-saved optim)) ; no reg, ε-mix=0
                      (cons "dual-gap/βsimplex" (duality-gap/βsimplex optim))))
  (define tot-cost (first (dict-ref cost-stats "Cost"))) ;; NOTICE: Assumes total is first column
  (define reg-cost (if reg (regularization-cost optim reg) 0.))
  (register 'cost-stats cost-stats) ; no regularization cost
  (register 'tot-cost tot-cost)
  (register 'reg-cost reg-cost)
  (register 'cost+reg (+ tot-cost reg-cost)))
