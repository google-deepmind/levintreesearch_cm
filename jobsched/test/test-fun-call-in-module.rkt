#lang racket
(require jobsched jobsched/fun-call
         rackunit)

;;; This test verifies that remote-call correctly embeds the module path,
;;; so start-remote-call-worker resolves the right function automatically.

(provide f)

(define (f x) (list 'f-result x))

(module+ worker
  (start-remote-call-worker))

(module+ test
  (define data (list (remote-call (f 'a))
                     (remote-call (f 'b))
                     (remote-call (f 'c))))
  (define results '())

  (start-simple-server #:worker-file (this-file)
                       #:data-list data
                       #:n-workers 2
                       #:process-result
                       (λ (data result)
                         (set! results (cons result results))))

  ;; Verify all results are correct.
  (check-equal? (sort results string<? #:key ~a)
                (sort (list '(f-result a) '(f-result b) '(f-result c))
                      string<? #:key ~a)))