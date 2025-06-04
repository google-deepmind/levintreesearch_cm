#lang racket
(require "../main.rkt")

;; This test will print some errors, but they will not be counted as such by raco test.

(module+ worker
  #;(displayln "early output") ; newline enforces message is sent
  (write 5) ; not self-delimiting!
  (start-simple-worker (λ (x) (writeln "yeaah"))))

(module+ test
  (require rackunit)
  (define data '(a b c))
  (check-exn
   exn:fail?
   ;; Server must exit gracefully despite the error.
   (λ () (start-simple-server #:worker-file (this-file)
                              #:data-list data
                              #:n-workers 2
                              #:process-result
                              (λ (data result)
                                (writeln (list 'got-result: result)))))))
