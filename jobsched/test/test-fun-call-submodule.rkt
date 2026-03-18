#lang racket/base
;;; Test that job:fun-call works with functions from submodules.

(require jobsched jobsched/fun-call)

(module helpers racket/base
  (provide helper-add)
  (define (helper-add x y) (+ x y)))

(module+ worker
  (start-fun-call-worker))

(module+ test
  (require rackunit
           (submod ".." helpers))
  (define data (list (job:fun-call (helper-add 1 2))
                     (job:fun-call (helper-add 10 20))))
  (define results '())

  (start-simple-server #:worker-file (this-file)
                       #:data-list data
                       #:n-workers 2
                       #:process-result
                       (λ (data result)
                         (set! results (cons result results))))

  (check-equal? (sort results <)
                '(3 30)))
