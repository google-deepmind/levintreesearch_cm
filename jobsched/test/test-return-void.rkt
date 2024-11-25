#lang racket
(require "../main.rkt")

;; (void) can be fasl'ed, so it shouldn't break.
(module+ worker (start-simple-worker (λ (x) (void))))

(module+ test
  (require rackunit)
  (define results '())
  (define data '(a b c))
  (start-simple-server #:worker-file (this-file)
                       #:data-list data
                       #:n-workers 2
                       #:process-result
                       (λ (data result) (set! results (cons result results))))
  (check-equal? results (make-list (length data) (void))))
