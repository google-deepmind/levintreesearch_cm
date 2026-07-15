#lang racket
;;; Test that remote-call raises a compile-time error when the function
;;; is not provided by any module.

(require rackunit)

;; This function is NOT provided — remote-call should refuse it at compile time.
(define (not-provided x) (list 'secret x))

(module+ test
  ;; Verify that using a non-provided function in remote-call raises a syntax error.
  (check-exn
   #rx"not provided by any module"
   (λ ()
     (eval '(begin
              (require jobsched/remote-call)
              (define (not-provided x) x)
              (remote-call (not-provided 42)))
           (make-base-namespace)))))
