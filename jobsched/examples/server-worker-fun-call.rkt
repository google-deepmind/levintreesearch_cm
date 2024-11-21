#lang racket
(require jobsched jobsched/fun-call)

;; The functions used by the server must be provided.
(provide foo bar)

;; Return values MUST be serializable via `fasl`.
(define (foo a #:? [b #f] c #:plop plip)
  (list a b plip c))

(define (bar argh)
  (list argh))

;; The worker automatically calls the relevant functions
(module+ worker
  (start-fun-call-worker (this-file))) ; (this-file) tells where to find the functions

(module+ main
  (define n-cpus 3)

  (start-simple-server
   #:worker-file (this-file) ; where to find the worker (in the 'worker submodule).
   ;; While these look like actual function calls (including the syntax checking
   ;; done by `define2`), they aren't, and will be evaluated on the worker.
   ;; The arguments are evaluated here though, and the resulting expressions must
   ;; be serializable via `fasl`.
   #:data-list (list (job:fun-call (foo 3 #:b 2 'c #:plop (+ 2 3 4)))
                     (job:fun-call (foo 'a 'cc #:plop 'oh))
                     (job:fun-call (bar 'bah)))
   #:process-result (λ (data result) (writeln result))
   #:n-proc n-cpus))