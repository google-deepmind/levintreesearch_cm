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

(require data/heap/unsafe
         racket/dict
         racket/fixnum
         "beta-matrix.rkt"
         "cdb.rkt"
         "context-setter.rkt"
         "flonum.rkt"
         "misc.rkt"
         "policy.rkt"
         define2)

(provide (struct-out bfs-node)
         bfs-node-last-action
         replay-actions
         result-solved?
         best-first-search
         make-bfs-solver)

;***************************************************************************************;
;****                               Best First Search                               ****;
;***************************************************************************************;

(define (d/π-cost-function ln1/π depth)
  (+ (log depth) ln1/π))

;====================;
;=== Search State ===;
;====================;

;; NOTICE:
;; * Since lists are partially shared between nodes, the cost
;;   of keeping all of act-seq and pcond-seq is not as high
;;   as one copy per node.
;;
;; state : any/c: the domain state
;; depth = (length act-seq) ; depth of root is 0.
(struct bfs-node (depth ln1/π act-seq state cost)
  #:transparent
  #:mutable)

(define (bfs-node-last-action state #:? [no-action #f])
  (define acts (bfs-node-act-seq state))
  (if (null? acts)
      no-action
      (car acts)))

;==============;
;=== Replay ===;
;==============;

;; Returns the resulting bfs-node, the sequence of probabilities, and the sequence of contexts.
;; Useful to retrieve a sequence of contexts from a cdb and a sequence of actions.
;; NOTICE: bfs-node-act-seq is in REVERSE order compared to the returned sequences
;;  of probabilities and contexts, and to act-seq.
(define (replay-actions domain-root-state
                        #:! do-action
                        #:! get-contexts/setter
                        #:? [ctx-vec #f]
                        #:! act-seq
                        #:! cdb
                        #:? [ε-mix 0.]
                        #:? [cost-function d/π-cost-function])
  (define get-pconds (make-cdb-predictor cdb))

  (for/fold ([nd (make-root-node domain-root-state cost-function)]
             [pcond-seq '()]
             [ctxs-seq '()]
             ;; TODO: Return a result directy
             #:result (values nd (reverse pcond-seq) (reverse ctxs-seq)))
            ([act (in-list act-seq)])
    (define-values (domain-state _) (apply values (bfs-node-state nd)))
    (define get-vec (if ctx-vec
                        (make-fxvector-setter ctx-vec)
                        (make-fxvector-setter/auto)))
    (get-contexts/setter domain-state nd get-vec)
    ;; We need to copy the vector otherwise it gets shared
    (define ctxs (if ctx-vec (fxvector-copy ctx-vec) (get-vec)))
    (define pconds           (get-pconds ctxs ε-mix))
    (define parent-ln1/π     (bfs-node-ln1/π     nd))
    (define parent-depth     (bfs-node-depth     nd))
    (define parent-act-seq   (bfs-node-act-seq   nd))
    (define new-domain-state (do-action domain-state act))
    (define pcond (flvector-ref pconds act))
    (define ln1/π (- parent-ln1/π (log pcond)))
    (define depth (+ parent-depth 1))
    (define cost (and cost-function (cost-function ln1/π depth)))
    (values
     (bfs-node depth
               ln1/π
               (cons act parent-act-seq)
               (list new-domain-state #f)
               cost)
     (cons pcond pcond-seq)
     (cons ctxs ctxs-seq))))

(define (result-solved? res)
  (eq? (dict-ref res 'status #f)
       'SOLVED))

;=========================;
;=== Best first search ===;
;=========================;

(struct visitor (hash [cuts #:mutable] [missed #:mutable])
  #:transparent)

(define (bfs-node-cost<=? ss1 ss2)
  (<= (bfs-node-cost ss1)
      (bfs-node-cost ss2)))

(define-syntax-rule (ids->dict id ...)
  (list (cons 'id id) ...))

(define (make-root-node root-state cost-function)
  (let ([1/π 1.] [depth 1])
    ;; Instead of pushing just the state, we push old-state+act, which can save a lot of time and
    ;; memory. For the root node however, we push the non-action.
    (bfs-node depth 1/π '() (list root-state #f) (cost-function 1/π depth))))

;; Returns an assoc, which is read/write-able
;; get-contexts/setter: MUST be functional, apart from the setter.
(define (best-first-search #:! root-state
                           #:! get-contexts/setter
                           #:! cdb
                           #:! ε-mix
                           #:? [cost-function d/π-cost-function]
                           #:! do-action ; state action -> state
                           #:! solution?
                           ;; Set to `#f` to disable transposition tables
                           #:? [visitor-make-hash make-hash]
                           #:? [get-visited-key #f] ; (or/c #f (-> bfs-node? any/c))
                           #:? [budget +inf.0]) ; expansion budget

  (define get-pconds (make-cdb-predictor cdb))

  (define q (make-heap bfs-node-cost<=?))
  (define root (make-root-node root-state cost-function))
  (heap-add! q root)

  (define ctxs
    (let ([n 0])
      ;; We need to find out the number of mutex sets
      ;; so as to create a vector of the correct size.
      ;; NOTICE: Assumes that the number of mutex sets is always the same during the whole search!
      (get-contexts/setter root-state root (λ _ (++ n)))
      (make-fxvector n)))

  (define get-contexts
    (if (CDB-ctx.idx-vec cdb)
        (λ (domain-state nd)
          (get-contexts/setter domain-state nd (make-fxvector-setter ctxs))
          ctxs)
        ;; There is no parameter matrix βmatrix, so for efficiency we can
        ;; skip obtain the contexts. This can be significantly faster on
        ;; the first iteration.
        (λ (domain-state nd)
          ;; The contents of ctxs does not matter since get-pconds will
          ;; return the uniform vector anyway.
          ctxs)))

  (define visited (visitor-make-hash))

  (define visited?!
    (if get-visited-key
        (λ (domain-state nd)
          (define 1/π (bfs-node-ln1/π nd))
          (define cost (bfs-node-cost nd))
          (define better? #false)
          (hash-update! visited
                        (get-visited-key domain-state nd)
                        (λ (old-info)
                          (define-values (old-cost old-1/π) (apply values old-info))
                          (set! better? (and (< cost old-cost)
                                             (<= 1/π old-1/π)))
                          (if better? (list cost 1/π) old-info))
                        (list +inf.0 +inf.0))
          (not better?))
        ;; Disable transposition tables
        (λ (domain-state sstate) #false)))

  (define expansions 0)
  (define generations 1) ; for the root
  (define start (current-milliseconds))

  ;; Returns an assoc, which is read/write-able
  (define (make-result #:! status #:? [act-seq #f])
    (define time-ms (- (current-milliseconds) start))
    ;; Replay the sequence of actions to obtain the sequence of contexts and the sequence of pconds.
    ;; This avoids having to store all contexts of all nodes during the search,
    ;; which can be very memory hungry.
    (cond [act-seq
           (define-values (_nd pcond-seq ctxs-seq)
             (replay-actions root-state
                             #:do-action do-action
                             #:get-contexts/setter get-contexts/setter
                             #:ctx-vec ctxs
                             #:cdb cdb
                             #:ε-mix ε-mix
                             #:act-seq act-seq
                             #:cost-function cost-function))
           (define len (length act-seq))
           (ids->dict time-ms generations expansions status act-seq pcond-seq ctxs-seq len)]
          [else
           (define pcond-seq #f)
           (define len #f)
           (define ctxs-seq #f)
           (ids->dict time-ms generations expansions status act-seq pcond-seq ctxs-seq len)]))

  (define res
    (let loop ()
      (cond
        [(= 0 (heap-count q))
         (make-result #:status #f)]
        [(>= expansions budget)
         (make-result #:status 'budget)]
        [else
         (define nd (heap-min q))
         (heap-remove-min! q)
         (define-values (parent-domain-state act) (apply values (bfs-node-state nd)))
         (define domain-state (if act
                                  (do-action parent-domain-state act)
                                  parent-domain-state))
         (cond
           [(solution? domain-state)
            (make-result #:status  'SOLVED
                         #:act-seq (reverse (bfs-node-act-seq   nd)))]
           [(visited?! domain-state nd)
            (loop)] ; skip
           [else
            ;; expand state to get children.
            (define ctxs           (get-contexts domain-state nd))
            (define pconds         (get-pconds ctxs ε-mix))
            (define parent-ln1/π   (bfs-node-ln1/π     nd))
            (define parent-depth   (bfs-node-depth     nd))
            (define parent-act-seq (bfs-node-act-seq   nd))
            ;; Note: invalid actions are not removed, and so
            ;; the action probabilities are *not* renormalized accordingly.
            ;; That is, the policy should learn to assign 0 probability to invalid actions.
            ;; (Though the effect of renormalization could be investigated too.)
            (for ([act (in-naturals)]
                  [pcond (in-flvector pconds)])
              (define ln1/π (- parent-ln1/π (log pcond)))
              (define depth (+ parent-depth 1))
              (define cost (cost-function ln1/π depth))
              ;; We  avoid the cost of do-action in the whole fringe
              ;; by delaying do-action until the node is actually visited.
              (define new-node
                (bfs-node
                 depth
                 ln1/π
                 (cons act parent-act-seq)
                 (list domain-state act)
                 cost))
              (heap-add! q new-node))
            (++ expansions)
            (+= generations (flvector-length pconds))
            (loop)])])))
  res)

;; Returns a domain-specific bfs solver
(define ((make-bfs-solver #:! n-actions
                          #:! get-contexts/setter
                          #:! get-visited-key
                          #:! solution?
                          #:! do-action)
         ;; Arguments that the domain-specific solver takes:
         root-state
         #:? [cdb (make-cdb n-actions)]
         #:? [get-contexts/setter get-contexts/setter]
         #:? [visited-hash? #t]
         #:? [budget +inf.0]
         #:? [ε-mix (*ε-mix*)])

  (best-first-search #:root-state root-state
                     #:get-contexts/setter get-contexts/setter
                     #:cdb cdb
                     #:ε-mix ε-mix
                     #:do-action do-action
                     #:solution? solution?
                     #:get-visited-key (and visited-hash? get-visited-key)
                     #:budget budget))
