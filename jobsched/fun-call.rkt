#lang racket/base

(require (for-syntax syntax/parse racket/base)
         "worker.rkt"
         racket/match
         racket/dict
         syntax/location
         define2)

(provide (struct-out jobsched:fun-call)
         job:fun-call
         start-fun-call-worker)

(struct jobsched:fun-call (fun-sym kw-dict pos-args) #:prefab)

(define-syntax (job:fun-call stx)
  (syntax-parse stx
    [(_ fun-call)
     #:with (fun:expr (~or* (~seq kw:keyword kw-arg:expr) arg2:expr) ...) #'fun-call
     #'(if #false
           ;; Check the syntax according to `define2`, but do not call (or even evaluate) the arguments.
           ;; This also requires `fun` to be provided/required to avoid a unknown identifier error.
           ;; (Not sure the required part is such a good thing, but at least this gives a static check.)
           ;; It's important that we use a syntax id here (not a list) so that error reporting within
           ;; DrRacket is at the call site, not in the macro.
           ;; (Even if the compiler removes this branch at compile-time, it's fine.)
           fun-call
           ;; Collect the evaluated arguments
           (jobsched:fun-call 'fun
                              (list (~? (cons 'kw kw-arg)) ...)
                              (list (~? arg2) ...)))]))

(define (start-fun-call-worker module-path)
  (define mod-path
    (cond [(and (string? module-path) (absolute-path? module-path)) (list 'file module-path)]
          [(module-path? module-path) module-path]
          [else (raise-argument-error 'start-fun-call-worker
                                      "A string or a module path"
                                      module-path)]))
  (start-simple-worker
   (match-lambda
     [(jobsched:fun-call fun-sym kw-dict pos-args)
      (define proc (dynamic-require mod-path fun-sym))
      (keyword-apply/dict proc kw-dict pos-args)]
     [jb (error "ill-formed job" jb)])))
