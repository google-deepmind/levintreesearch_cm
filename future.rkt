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

(require racket/future
         racket/list
         racket/fixnum
         "flonum.rkt"
         define2)

(#%declare #:unsafe) ; Warning: Can be faster, but some bugs may become silent

(provide partition-range
         flvectors-sum/async)

;; Returns the list of start indices and the list of end indices.
;; We spread the remainder on the first rem segments,
;; so that each segment is of size either segment-len or segment-len + 1.
;; This shares the load more uniformly. For example:
;; > (partition-range 50 7)
;; '(43 36 29 22 15 8 0)
;; '(50 43 36 29 22 15 8)
;; which has 6 segments of 7 elements, and one segment of 8 elements.
;; By contrast, a more naive partitioning based on (exact-ceiling (/ len n-segments)) would give:
;; '(48 40 32 24 16 8 0)
;; '(50 48 40 32 24 16 8)
;; which has 6 segments of 8 elements, and 1 segment of 2 elements.
(define (partition-range len n-segments)
  (define-values (segment-len rem) (quotient/remainder len n-segments))
  (let loop ([start 0] [i 0] [res '()])
    (cond
      [(= start len)
       ;; In reverse order but the order doesn't matter.
       (values (map first res) (map second res))]
      [else
       (define end (min len (+ start segment-len (if (< i rem) 1 0))))
       (loop end (+ i 1) (cons (list start end) res))])))

(module+ test
  (require rackunit)
  (check-equal? (call-with-values (λ () (partition-range 100 7)) list)
                '((86 72 58 44 30 15 0)
                  (100 86 72 58 44 30 15)))
  (check-equal? (call-with-values (λ () (partition-range 7 100)) list)
                '((6 5 4 3 2 1 0)
                  (7 6 5 4 3 2 1)))
  (check-equal? (call-with-values (λ () (partition-range 0 100)) list)
                '(()
                  ())))


(define (flvectors-sum/async flvecs #:? [n-futures (processor-count)] #:? [vec-out #f])
  (define flvecsv
    (cond [(and (vector? flvecs) (for/and ([v (in-vector flvecs)]) (flvector? v))) flvecs]
          [(and (list? flvecs) (andmap flvector? flvecs)) (apply vector flvecs)]
          [else (raise-argument-error 'flvectors-sum/async "(or/c (listof flvector) (vectorof flvector))" flvecs)]))
  (unless (or (not vec-out) (flvector? vec-out))
    (raise-argument-error 'flvectors-sum/async "flvector?" flvecs))
  (unless (exact-positive-integer? n-futures)
    (raise-argument-error 'flvectors-sum/async "exact-positive-integer?" n-futures))

  (define len (flvector-length (vector-ref flvecsv 0)))
  (define vout (or vec-out (make-flvector len 0.)))
  (define-values (starts ends) (partition-range len n-futures))
  (for/async ([start (in-list starts)]
              [end (in-list ends)])
    (for ([i (in-range start end)])
      (define s (for/flsum ([v (in-vector flvecsv)]) (flvector-ref v (fx+ i))))
      (flvector-set! vout (fx+ i) (fl+ s))))
  vout)

;; To try this out:
;;   racket -l- lts-cm/future
;; Be patient.
(module+ main
  (require rackunit
           "misc.rkt")

  (define (test-merge n-futures n-vec len)
    (writeln (ids->assoc n-futures n-vec len))
    (displayln "Creating vectors")
    (define flvecs
      (time
       (for/list ([i n-vec])
         (for/flvector #:length len ([j len])
           (random)))))

    (displayln "Slow res (no future)")
    (define slow-res
      (time
       (for/flvector #:length len ([i (in-range len)])
         (for/flsum ([v (in-list flvecs)])
           (flvector-ref v i)))))

    (displayln "Fast res")
    (collect-garbage)
    (define fast-res (time (flvectors-sum/async flvecs)))

    ;; In case the time necessary to create vout can be amortized, the gain may be more substantial.
    (displayln "Fast res (without allocation)")
    (define vout (make-flvector len 0.))
    (collect-garbage)
    (collect-garbage)
    (define fast2-res (time (flvectors-sum/async flvecs #:vec-out vout)))

    (check-within fast-res slow-res 1e-5)
    (check-within fast2-res slow-res 1e-5)


    (displayln "Fast res (no alloc) for various number of futures")
    (for ([n-futures (in-range 1 (+ 1 (processor-count)))])
      (printf "n: ~a " n-futures)
      (time (flvectors-sum/async flvecs #:vec-out vout #:n-futures n-futures))))

  (test-merge (processor-count) 2 100000000)
  ;; Results with (#%declare #:unsafe)
  ;; (creation time: 18s)
  ;; slow res: 1900ms
  ;; fast res:  293ms
  ;; fast res no allocation:
  ;; n: 1   cpu time: 597  real time: 597 gc time: 0
  ;; n: 2   cpu time: 590  real time: 298 gc time: 0
  ;; n: 3   cpu time: 597  real time: 200 gc time: 3
  ;; n: 4   cpu time: 591  real time: 149 gc time: 0
  ;; n: 5   cpu time: 622  real time: 132 gc time: 0
  ;; n: 6   cpu time: 682  real time: 188 gc time: 0
  ;; n: 7   cpu time: 633  real time: 103 gc time: 4
  ;; n: 8   cpu time: 599  real time: 77  gc time: 0
  ;; n: 9   cpu time: 610  real time: 74  gc time: 0
  ;; n: 16  cpu time: 1264 real time: 135 gc time: 0
  ;; n: 32  cpu time: 966  real time: 67  gc time: 5
  ;; n: 64  cpu time: 1492 real time: 43  gc time: 6
  ;; n: 128 cpu time: 1898 real time: 32  gc time: 0
  ;; That's a gain of a factor 60(!)

  #;(test-merge (processor-count) 64 32000000)
  )

