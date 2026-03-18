#lang racket
(require jobsched
         jobsched/fun-call)

;; The functions used by the server must be provided.
(provide foo bar)

;; Return values MUST be serializable via `fasl`.
(define (foo a #:b [b #f] c #:plop plip)
  (list a b plip c))

(define (bar argh)
  (list argh))

;; The worker automatically calls the relevant functions. No argument needed —
;; the module path is embedded in each job struct by remote-call.
(module+ worker (start-remote-call-worker))

(module+ main
  (define n-workers 3)

  (start-simple-server
   ;; The worker is defined in the 'worker submodule of this file.
   #:worker-file (this-file)
   ;; While these look like actual function calls (including the syntax checking
   ;; done by `define2`), they aren't, and will be evaluated on the worker.
   ;; The arguments are evaluated here though, and the resulting expressions must
   ;; be serializable via `fasl`.
   ;; Try for example to remove or add a keyword argument, and observe DrRacket
   ;; complaining immediately.
   #:data-list (list (remote-call (foo 3 #:b 2 'c #:plop (+ 2 3 4)))
                     (remote-call (foo 'a 'cc #:plop 'oh))
                     (remote-call (bar 'bah)))
   #:process-result (λ (data result) (writeln result))
   #:n-workers n-workers))
