#lang racket
(require jobsched)

;=== Worker ===;

(define (worker-run data)
  (match data
    [(list x y)
     (* x y)]))

(module+ worker
  (start-simple-worker worker-run))

;=== Server ===;

(module+ main
  (require racket/runtime-path)

  (define-runtime-path this-file "server-worker-simple.rkt")

  (define (process-result data result)
    (printf "~a × ~a = ~a\n" (first data) (second data) result))

  (define data-list
    (for*/list ([x 5] [y 5]) (list x y)))
  
  (start-simple-server
   #:worker-file this-file
   #:data-list data-list
   #:process-result process-result))
