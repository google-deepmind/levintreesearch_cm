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

(require racket/math
         racket/match
         racket/list
         racket/dict
         "misc.rkt"
         define2)

(provide (struct-out pt)
         (struct-out ptg)
         intersection
         convex-line-search
         quasi-exact-line-search)

;**************************************************************************************;
;****                Delta-Secant: Line Search For Convex Functions                ****;
;**************************************************************************************;

(struct pt (x y) #:transparent)
(struct ptg pt (g) #:transparent) ; g is the gradient, if available

;=======================;
;=== General helpers ===;
;=======================;

(define (nanfinite? x) (or (nan? x) (infinite? x)))

;==========================;
;=== Geometry functions ===;
;==========================;

;; Returns xq if xq is far away enough from xmid, otherwise
;; returns a point that's slightly further to avoid numerical instability.
;; We use a power of 2 to touch only the exponent bits of the floats and leave the mantissa alone.
;; xq, x-, xmid, x+: real?
;; u: positive-real?
;; -> real?
(define (xmid-repulsion xq x- xmid x+ #:u [u (expt 2 -7)])
  (define a (+ xmid (* u (- x- xmid)))) ; more stable than (+ (* u xmid) (* (- 1 u) x-))
  (define b (+ xmid (* u (- x+ xmid))))
  (cond [(<= a xq xmid) a]
        [(>= xmid xq b) b]
        [else           xq]))

;; Returns the gradient at p1 if available (i.e., if p1 is a ptg), or the slope between
;; p1 and p2.
;; If p1 = p2, nan is returned.
(define (slope/gradient p1 p2)
  (if (ptg? p1)
      (ptg-g p1)
      (if (= (pt-x p1) (pt-x p2))
          (if (= (pt-y p1) (pt-y p2))
              +nan.0 ; same point, unknown slope
              +inf.0)  ; vertical line
          (/ (- (pt-y p1) (pt-y p2))
             (- (pt-x p1) (pt-x p2))))))

;; Returns the x coordinate of the intersection of the line going through p1 and p2 and crossing the
;; y axis at y.
;; Assumes: y2 ≥ y1 ≥ y
;; p1, p2: pt?
;; y: real?
;; -> real?
(define (ppy-x-intersection p1 p2 y)
  (match-define (pt x1 y1) p1)
  (match-define (pt x2 y2) p2)
  (define a (slope/gradient p1 p2))
  (cond [(or (zero? a) ; bit of a hack, but works for use cases
             (nanfinite? a)
             (nanfinite? (- y y1)))
         x1]
        [else (+ x1 (/ (- y y1) a))]))

;; Line-line intersection, where the first line goes through p1 and p2, and the second line
;; goes through p3 and p4. Assumes x1≤x2≤x3≤x4 and y1≥y2, y3≤y4
;; pts: (list/c pt? pt? pt? pt?)
;; -> pt?
(define (intersection pts)
  (match-define (list p1 p2 p3 p4) pts)
  (match-define (list (pt x1 y1) (pt x2 y2) (pt x3 y3) (pt x4 y4)) pts)

  (define a2 (slope/gradient p2 p1))
  (define a3 (slope/gradient p3 p4))
  (define (f2 x) (+ y2 (* a2 (- x x2))))
  (define (f3 x) (+ y3 (* a3 (- x x3))))
  (cond [(and (nanfinite? a2) (nanfinite? a3))
         (pt (* .5 (+ x2 x3)) -inf.0)]
        [(nanfinite? a2)
         (pt x2 (f3 x2))]
        [(nanfinite? a3)
         (pt x3 (f2 x3))]
        [(= a2 a3)
         (eprintf "warning: parallel lines: ~a\n" pts)
         (argmin pt-y pts)]
        [else
         ;; We need to be a little careful about numerical stability here.
         ;; for all x', we have
         ;; xcross = x' + [f3(x') - f2(x')] / (a2-a3) .
         ;; We can thus substitute x' by the best approximation of xcross, and hopefully reduce
         ;; the error. If x'=xcross, then f3(x')=f2(x') = ycross
         ;; After 3 iterations, we return
         (let loop ([iter 0] [xcross x2])
           (define yc2 (f2 xcross))
           (define yc3 (f3 xcross))
           (define new-xcross (+ xcross (/ (- yc3 yc2) (- a2 a3))))
           (if (= iter 3)
               (pt xcross (min yc2 yc3)) ; we take the min to upper bound the y-gap
               (loop (+ iter 1) new-xcross)))]))

;; Assumes that at most either y0 or y3 is infinite, the rest must be finite.
;; p0, p1, p2, p3: pt?
;; -> real?
(define (p4-y-intersection p1 p2 p3 p4)
  (pt-y (intersection (list p1 p2 p3 p4))))

;==========================;
;=== Convex line search ===;
;==========================;

;; Insert pq in the five-point list pts. Assumes that xq in [x1, x3].
;; pq: pt?
;; pts: (list/c pt? pt? pt? pt? pt?) ; a five-point list
(define (insert-pt pq pts)
  (match-define (list p0 p1 p2 p3 p4) pts)
  (match-define (list (pt x0 y0) (pt x1 y1) (pt x2 y2) (pt x3 y3) (pt x4 y4)) pts)
  (match-define (pt xq yq) pq)
  (assert (and (<= x0 x1 x2 x3 x4) (<= x1 xq x3)) pts pq)
  (if (< xq x2)
      (if (< yq y2)
          (list p0 p1 pq p2 p3)
          (list p1 pq p2 p3 p4))
      (if (< yq y2)
          (list p1 p2 pq p3 p4)
          (list p0 p1 p2 pq p3))))

(define (Δx pts)
  (match-define (list p0 p1 p2 p3 p4) pts)
  (match-define (list (pt x0 y0) (pt x1 y1) (pt x2 y2) (pt x3 y3) (pt x4 y4)) pts)
  ;; x-axis value of line intersection with y axis
  (define x- (min (max x1 (ppy-x-intersection p0 p1 y2)) x2))
  (define x+ (min (max x2 (ppy-x-intersection p4 p3 y2)) x3))
  (values x- x2 x+))

(define (Δy pts)
  (match-define (list p0 p1 p2 p3 p4) pts)
  (match-define (list (pt x0 y0) (pt x1 y1) (pt x2 y2) (pt x3 y3) (pt x4 y4)) pts)
  (define ya (p4-y-intersection p0 p1 p2 p3)) ; y-axis value of line-line intersection
  (define yb (p4-y-intersection p1 p2 p3 p4))
  (values ya y2 yb))

;; f: real? -> real?
;;   f MUST be convex inside [xleft, xright]
;; pts: (list/c pt? pt? pt? pt? pt?)
;;   The points pts MUST be in increasing order of pt-x.
;;   The pt-y of the middle point MUST be the minimum of the 5 points.
;; stop-when: (-> dict? any/c)
;;   Stopping criterion. The algorithm returns when not #f.
;;   The input dictionary has the same keys as the result dictionary (see below)
;; callback: (-> dict any)
;;   May be used to display information during the line search.
;;   The input dictionary has the same keys as the result dictionary (see below)
;; -> dict?
;;   Keys of the returned dictionary:
;;   'iter: number of queries within convex-line-search* (may not include previous queries)
;;   'pts: 5 points around the minimum
;;   'x-, 'x+: boundaries of the x*-gap Δx
;;   'ya, 'yb: y-values of the two bottom vertices of the optimality triangles
;;   'xgap, 'ygap: |Δx| and |Δy|
;;   'xlow, 'ylow: lowest queried point
;; Note: See `define-assoc` to deal with the dictionaries.
(define (convex-line-search* f pts
                             #:! stop-when
                             #:? [callback (λ (dic) (void))])
  (let loop ([iter 1] [pts pts])
    (match-define (list p0 p1 p2 p3 p4) pts)
    (match-define (list (pt x0 y0) (pt x1 y1) (pt x2 y2) (pt x3 y3) (pt x4 y4)) pts)

    (define-values (x- xlow x+) (Δx pts)) ; xlow = x2
    (define-values (ya ylow yb) (Δy pts)) ; ylow = y2
    (define xgap (- x+ x-))
    (define ygap (max 0. (- y2 ya) (- y2 yb)))

    (define dic (ids->assoc iter pts x- x+ xgap ya yb ygap xlow ylow))
    (callback dic)
    (cond [(stop-when dic) dic]
          [else
           ;; Query in the middle of the x*-gap,
           ;; but force xq to be slightly away from x2 if necessary.
           (assert (<= (pt-x p1) x- x2 x+ (pt-x p3)) pts x- x+)
           (define xq (xmid-repulsion (* .5 (+ x- x+)) x- x2 x+)) ;  x1 <= x- <= xq <= x+ <= x3
           (assert (<= x- xq x+) x- xq x+)
           (define new-pts (insert-pt (pt xq (f xq)) pts))
           (loop (+ iter 1) new-pts)])))

;; f, stop-when, callback: see `convex-line-search*`
;; xleft, xright: real?
;; xq: real? ∈ (xleft, xright)
;; yq: real? ; If provided it MUST be equal to (f xq)
;; y-tolerance: positive-real?
;;   Used to define the default stopping criterion.
;;   By default the algorithm returns when the y*-gap |Δy| is no more than the y-tolerance.
;; -> dict?
;;   See `convex-line-search*`.
(define (convex-line-search f xleft xright
                            #:? [yleft (f xleft)]
                            #:? [xq (* .5 (+ xleft xright))]
                            #:? [yq (f xq)]
                            #:? [y-tolerance 1.e-10]
                            #:? [stop-when (λ (dic) (<= (dict-ref dic 'ygap) y-tolerance))]
                            #:? [callback (λ (dic) (void))])
  (define pts
    (insert-pt (pt xq yq)
               ; no need to query at xright if yq > yleft
               (insert-pt (pt xright (if (> yq yleft) +inf.0 (f xright)))
                          (list (pt xleft +inf.0)
                                (pt xleft +inf.0)
                                (pt xleft yleft)
                                (pt xright +inf.0)
                                (pt xright +inf.0)))))
  (convex-line-search* f pts #:stop-when stop-when #:callback callback))

;; Quasi-exact line search
;; f, xleft, xright, xq, yq, callback: see `convex-line-search`
;; jac^2: real? ; L2-norm of the Jacobian. Can provide additional information to speed up the search.
;; c: positive-real? ; algorithm parameter. As c tends to ∞, the line search becomes more exact.
;; Features:
;; * Uses initial gradient information to speed up the search a little
;; * Avoids one query at xright if f(xq) ≥ f(xleft)
;; * Quadruples the range [xleft, xright] if the maximum is at xright and starts the line search
;;   again, re-using previous points. This ensures adaptation to the strong-convexity paramatere M
;;   when M < 1.
;;
;; Notice: Initially I was building two additional virtual points to mimic the gradient at 0.
;;   However this appeared to be numerically unstable for exponentials.
;;   Instead, we keep the gradient as is, and use an extended point ptg.
;;   The `slope/gradient` function takes care of using it whenever possible.
;;   This ensures at least that the tangent gives the exact value at 0.
(define (quasi-exact-line-search f [xleft 0.] [xright 1.]
                                 #:? [yleft (f xleft)]
                                 #:? [xq (* .5 (+ xleft xright))]
                                 #:? [yq (f xq)]
                                 #:? [jac^2 #f]
                                 #:? [c 1.]
                                 #:? [callback (λ (dic) (void))])
  (define pts
    (insert-pt (pt xq yq)
               ; no need to query at xright if yq > yleft
               (insert-pt (pt xright (if (> yq yleft) +inf.0 (f xright)))
                          (list (pt xleft +inf.0)
                                (pt xleft +inf.0)
                                (if jac^2 (ptg xleft yleft (- jac^2)) (pt xleft yleft))
                                (pt xright +inf.0)
                                (pt xright +inf.0)))))

  (define (stop-when dic)
    (>= (- yleft (dict-ref dic 'ylow))
        (* c (dict-ref dic 'ygap))))

  (let loop ([iter 0] [pts pts] [xright xright])
    (define res (convex-line-search* f pts #:stop-when stop-when #:callback callback))
    (define new-pts (dict-ref res 'pts))
    (cond [(= xright (dict-ref res 'xlow))
           ;; Minimum is at the xright boundary, quadruple the range and try again
           ;; with the current information
           (define new-xright (* 4. xright))
           (when (and (> iter 50) (= (expt 2 (exact-ceiling (log iter 2))) iter))
             (writeln (ids->assoc iter new-xright)))
           (loop (+ iter 1)
                 (insert-pt (pt new-xright (f new-xright))
                            (append (take new-pts 3)
                                    (list (pt new-xright +inf.0)
                                          (pt new-xright +inf.0))))
                 new-xright)]
          [else res])))

;============================;
;=== Results of the paper ===;
;============================;

(module+ drracket
  (define (f->fcount f)
    (let ([n 0])
      (case-lambda
        [() n]
        [(x) (set! n (+ n 1)) (f x)])))

  (define results '())

  (define (convex-line-search/count f xleft xright)
    (define fcount (f->fcount f))
    (define res (convex-line-search fcount xleft xright))
    (define new-res (cons (cons 'queries (fcount)) res))
    (set! results (cons new-res results))
    new-res)

  (convex-line-search/count + -20. 7.) ; 3 queries
  ;;
  (convex-line-search/count - -20. 7.) ; 3 queries
  (convex-line-search/count abs -20. 7.) ; 7 queries
  (convex-line-search/count (lambda (x) (max (- x) (* 2 x))) -20. 7.) ; 23 queries
  (convex-line-search/count (lambda (x) (max (- x) (* 2 x))) -0.01 100.) ; 18 queries
  (convex-line-search/count (lambda (x) (expt (abs x) 1.1)) -20. 7.) ; 28 queries
  (convex-line-search/count sqr -20. 7.) ; 27 queries
  (convex-line-search/count (lambda (x) (sqrt (+ 1 (sqr x)))) -1000. 900.) ; 23 queries
  (convex-line-search/count (lambda (x) (- (* x (log x)) x)) 0.001 20.) ; 23 queries
  (convex-line-search/count (lambda (x) (max (sqr x)
                                             (sqr (- x 3)))) -5. 55.) ; 18 queries
  (convex-line-search/count (lambda (x) (max (sqr x)
                                             (sqr (- (* .5 x) 3)))) -5. 55.) ; 26 queries
  (convex-line-search/count (lambda (x) (expt x 4)) -20. 7.) ; 18 queries
  (convex-line-search/count (lambda (x) (+ (sqr x) (/ (sqr x)))) 0.001 100.) ; 31 queries
  ;;
  (convex-line-search/count (lambda (x) (max x (- x) (- (* 2 x) 3))) -20. 7.) ; 9 queries

  (define res-queries (map (λ (d) (dict-ref d 'queries)) (reverse results)))
  (writeln res-queries)
  (require rackunit)
  (check-equal? res-queries
                '(3 3 7 23 18 28 27 23 23 18 26 18 31 9))
  )

;=============;
;=== Tests ===;
;=============;

(module+ test
  (require rackunit)
  ;; These should not raise exception or output nans
  (apply p4-y-intersection (list
                            (pt -2.0 1.44519475362515e+87)
                            (pt -1.0 7.22597376812575e+86)
                            (pt 0.0 2.6881171418161356e+43)
                            (pt 2.2958874039497803e-41 3.9843245232693915e+224)))

  (apply p4-y-intersection (list (pt -2.0 5.658533273155181e+22)
                                 (pt -1.0 2.8292666365860003e+22)
                                 (pt 0.0 168204240034.82135)
                                 (pt 3.7252901874396124e-9 8.077124387443765e+260)))

  (apply p4-y-intersection (list (pt -1.0 3.5410470781288975e+158)
                                 (pt 0.0 5.809105214221176e+78)
                                 (pt 8.636168555094445e-78 2.615187575282842e+112)
                                 (pt 1.727233711018889e-77 8.93004528949309e+268)))

  (apply p4-y-intersection (list (pt -281474976710657.94 96055693738562660.0)
                                 (pt -1.951968428709672 666.1255780765824)
                                 (pt -1.936343428709672 0.0)
                                 (pt -1.936343428709672 0.0)))

  (apply p4-y-intersection (list (pt -5.5511151231257815e-17 +inf.0)
                                 (pt 0.0 2.802156757109936e+38)
                                 (pt 2.465190328815662e-32 +inf.0)
                                 (pt 1.110223024625157e-16 +inf.0)))

  (p4-y-intersection (pt 0.0 +inf.0)
                     (pt 0.0 +inf.0)
                     (pt 0.0 2.819518316088045e+80)
                     (pt 5.397605346934028e-79 2.3871438988221628e+259))

  (ppy-x-intersection (pt 4.1359030627651384e-25 5.3840574518170934e+23)
                      (pt 4.1359030627651384e-25 5.3840574518170934e+23)
                      5.3840574518170934e+23)

  (let ()
    (define pts
      (list
       (pt -1.0                    8.072971600664251e+33)
       (pt -5.5511151231257815e-17 15560063104485898000.0)
       (pt 0.0                       159798090027112200.0)
       (pt 1.232595164407831e-32     159798090027112200.0)
       (pt 2.465190328815662e-32     159798090027112200.0)))
    (match-define (list p0 p1 p2 p3 p4) pts)
    (match-define (list (pt x0 y0) (pt x1 y1) (pt x2 y2) (pt x3 y3) (pt x4 y4)) pts)

     ; x-axis value of line intersection with y axis
    (define x- (min (max x1 (ppy-x-intersection p0 p1 y2)) x2))
    (define x+ (min (max x2 (ppy-x-intersection p4 p3 y2)) x3))
    (check-false (nan? x-))
    (check-false (nan? x+))
    (list x- x+))

  (let ()
    (define (2intersections pts)
      (list (intersection (map (λ (a) (apply pt a)) (take pts 4)))
            (intersection (map (λ (a) (apply pt a)) (rest pts)))))
    (2intersections '((7.831848664870608 23.50440106011682)
                         (7.832206898140632 23.496634662345294)
                         (7.832207474533485 23.496622423600456)
                         (7.832207474535078 23.496622423605235)
                         (7.832207497557746 23.496622492673236)))
    (2intersections '((98.99993425536616 0.010101016808966178)
                         (98.99998074454871 0.010101012065651978)
                         (98.999994360923 0.010101010676367445)
                         (99.44766334435741 4476633.443574088)
                         (100.0 10000000.0)))))
