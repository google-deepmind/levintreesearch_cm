#lang racket
(require jobsched)

;; Functions used by the server must be `provide`d.
(provide greet total)

(define (greet name #:greeting [greeting "Hello"])
  (format "~a, ~a!" greeting name))

(define (total prices #:tax-rate [tax-rate 0])
  (define subtotal (apply + prices))
  (exact->inexact (* subtotal (+ 1 tax-rate))))

;;=== Worker ===;;

;; The worker resolves and calls the functions automatically.
(module+ worker (start-remote-call-worker))

;;=== Server ===;;

(module+ main
  (start-simple-server
   #:worker-file (this-file)
   ;; These look like function calls, but they are captured and
   ;; sent to the workers for execution. Arguments are evaluated
   ;; here; the function itself is resolved on the worker.
   #:data-list (list (remote-call (greet "Alice"))
                     (remote-call (greet "Bob" #:greeting "Hi"))
                     (remote-call (total '(10 20 30)))
                     (remote-call (total '(100 200) #:tax-rate 0.2)))
   #:process-result (λ (data result) (displayln result))
   #:n-workers 3))
