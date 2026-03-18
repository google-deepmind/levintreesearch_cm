#lang racket/base
;;; Test that remote-call correctly resolves library functions and primitives.
;;; Uses #lang racket/base so the worker submodule does NOT have racket/list.

(require jobsched jobsched/fun-call)

(module+ worker
  ;; Only has racket/base — no racket/list.
  (start-remote-call-worker))

(module+ test
  (require rackunit
           racket/list) ; `first` is only available here, not in the worker

  ;; Test 1: `first` from racket/list
  ;; Test 2: `+` primitive from #%runtime
  (define data (list (remote-call (first '(10 20 30)))
                     (remote-call (first '(a b c)))
                     (remote-call (+ 1 2 3))
                     (remote-call (+ 100 200))))
  (define results '())

  (start-simple-server #:worker-file (this-file)
                       #:data-list data
                       #:n-workers 2
                       #:process-result
                       (λ (data result)
                         (set! results (cons result results))))

  (check-equal? (sort results string<? #:key (λ (x) (format "~a" x)))
                (sort '(10 a 6 300) string<? #:key (λ (x) (format "~a" x)))))
