#lang racket
(require "../main.rkt"
         racket/os)

;; This is not a proper test yet.

(module+ worker
  (start-simple-worker (λ (x)
                         (eprintf "User, break the program when ready.")
                         (sleep 1000))))

(module+ main
  (printf "PID: ~a\n" (getpid))
  (start-simple-server #:worker-file (this-file)
                       #:data-list '(a b c)
                       #:n-workers 3
                       #:process-result
                       (λ (data result)
                         (writeln (list 'got-result: result)))))
