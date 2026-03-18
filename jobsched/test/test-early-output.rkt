#lang racket
(require "../main.rkt")

;; With TCP communication, early output from the worker doesn't break the protocol
;; because the protocol goes over TCP, not stdout.

(module+ worker
  (displayln "early output") ; This is now harmless — goes to stderr, not the protocol channel
  (write 5) ; Also harmless
  (start-simple-worker (λ (x) (list 'result x))))

(module+ test
  (require rackunit)
  (define data '(a b c))
  (define results '())
  ;; This should now succeed — early output no longer breaks the server.
  (start-simple-server #:worker-file (this-file)
                       #:data-list data
                       #:n-workers 2
                       #:process-result
                       (λ (data result)
                         (set! results (cons result results))))
  (check-equal? (sort results string<? #:key ~a)
                (sort (map (λ (x) (list 'result x)) data) string<? #:key ~a)))
