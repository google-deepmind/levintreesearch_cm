#lang racket/base
;;; Test that job:fun-call correctly resolves library functions.
;;; The file uses #lang racket/base, so the worker submodule does NOT
;;; have access to racket/list. Only the test submodule imports it.
;;; The worker resolves `first` via dynamic-require using the module
;;; path embedded in each job struct by job:fun-call.

(require jobsched jobsched/fun-call)

(module+ worker
  ;; Only has racket/base — no racket/list.
  (start-fun-call-worker))

(module+ test
  (require rackunit
           racket/list) ; `first` is only available here, not in the worker

  ;; Test 1: `first` from racket/list
  ;; Test 2: `+` primitive from #%runtime
  (define data (list (job:fun-call (first '(10 20 30)))
                     (job:fun-call (first '(a b c)))
                     (job:fun-call (+ 1 2 3))
                     (job:fun-call (+ 100 200))))
  (define results '())

  (start-simple-server #:worker-file (this-file)
                       #:data-list data
                       #:n-workers 2
                       #:process-result
                       (λ (data result)
                         (set! results (cons result results))))

  (check-equal? (sort results string<? #:key (λ (x) (format "~a" x)))
                (sort '(10 a 6 300) string<? #:key (λ (x) (format "~a" x)))))
