#lang racket/base

(require (for-syntax syntax/parse racket/base)
         "worker.rkt"
         racket/match
         racket/dict
         define2)

(provide job:fun-call
         start-fun-call-worker)

(define JOBSCHED:FUN-CALL 'JOBSCHED:FUN-CALL)

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
           (list JOBSCHED:FUN-CALL
                 'fun
                 (list (~? (cons 'kw kw-arg)) ...)
                 (list (~? arg2) ...)))]))

(define (start-fun-call-worker file-path)
  (define file-path-string
    (cond [(string? file-path) file-path]
          [(path? file-path) (path->string file-path)]
          [else (raise-argument-error 'start-fun-call-worker
                                      "A string or a path"
                                      file-path)]))
  (start-simple-worker
   (match-lambda
     [(list JOBSCHED:FUN-CALL fun kw-args pos-args)
      (define proc (dynamic-require (list 'file file-path-string) fun))
      (keyword-apply/dict proc kw-args pos-args)]
     [jb (error "illformed job" jb)])))
