#lang racket/base

(require (for-syntax syntax/parse racket/base racket/list)
         "worker.rkt"
         racket/match
         racket/dict
         syntax/location
         define2)

(provide remote-call            ; the macro
         remote-call?           ; predicate
         remote-call-mod-path   ; accessor
         remote-call-fun-sym    ; accessor
         remote-call-kw-dict    ; accessor
         remote-call-pos-args   ; accessor
         start-remote-call-worker)

;; mod-path: a module path suitable for `dynamic-require`
;;   e.g., '(file "/abs/path") for user modules
;; fun-sym: the symbol name of the function
;; kw-dict: association list of keyword arguments
;; pos-args: list of positional arguments
(struct remote-call (mod-path fun-sym kw-dict pos-args)
  #:prefab
  #:constructor-name make-remote-call
  #:name jobsched:remote-call)

;; At compile time, extract the module path and symbol for an identifier,
;; suitable for `dynamic-require` at runtime.
(define-for-syntax (id->mod-path+sym id)
  (define binding (identifier-binding id))
  (cond
    [(or (not binding) (eq? binding 'lexical))
     ;; Local or lexical binding — the function is not exported from any module.
     ;; dynamic-require won't be able to find it, so raise a compile-time error.
     (raise-syntax-error 'remote-call
                         (format (string-append
                                  "function `~a` is not provided by any module. "
                                  "Add (provide ~a) to the module that defines it, "
                                  "so the worker can resolve it via dynamic-require.")
                                 (syntax-e id) (syntax-e id))
                         id)]
    [else
     ;; Imported binding.
     ;; identifier-binding returns:
     ;;   (list src-mod src-sym nom-mod nom-sym src-phase import-phase nom-export-phase)
     ;; Use nominal module + nominal symbol — this is how the user imported the function,
     ;; and it's what dynamic-require expects.
     (define nom-mpi (list-ref binding 2))
     (define nom-sym (list-ref binding 3))
     (define resolved (module-path-index-resolve nom-mpi))
     (define nom-name (resolved-module-path-name resolved))
     (cond
       [(path? nom-name)
        ;; Library or external module — use (file abs-path) form for dynamic-require.
        (values (list 'file (path->string nom-name)) nom-sym)]
       [(symbol? nom-name)
        ;; Self-module (function defined in same file) — use syntax-source.
        (define src (syntax-source id))
        (if (path? src)
            (values (list 'file (path->string src)) nom-sym)
            (values nom-name nom-sym))]
       [(list? nom-name)
        ;; Submodule — nom-name is e.g. '(mod-name sub-name ...).
        ;; The first element is the parent module (symbol or path).
        ;; Convert to (submod (file "/abs/path") sub-name ...) form.
        (define parent (car nom-name))
        (define sub-names (cdr nom-name))
        (define parent-path
          (cond [(path? parent) (path->string parent)]
                [(symbol? parent)
                 ;; Symbolic parent — resolve via syntax-source.
                 (define src (syntax-source id))
                 (if (path? src) (path->string src)
                     (error 'remote-call
                            "Cannot resolve submodule path for ~a" (syntax-e id)))]
                [else (error 'remote-call
                             "Unexpected submodule parent type: ~v" parent)]))
        (values (list* 'submod (list 'file parent-path) sub-names) nom-sym)]
       [else
        (values nom-name nom-sym)])]))

;; Start a remote-call worker. No arguments needed — the module path
;; is embedded in each job struct by the `remote-call` macro.
;; Defined BEFORE the macro so that `match` can use the struct info.
(define (start-remote-call-worker)
  (start-simple-worker
   (match-lambda
     [(jobsched:remote-call mod-path fun-sym kw-dict pos-args)
      (define proc (dynamic-require mod-path fun-sym))
      (keyword-apply/dict proc kw-dict pos-args)]
     [jb (error "ill-formed remote-call job" jb)])))

;; The macro — shadows the struct binding for `remote-call`.
(define-syntax (remote-call stx)
  (syntax-parse stx
    [(_ fun-call)
     #:with (fun:expr (~or* (~seq kw:keyword kw-arg:expr) arg2:expr) ...) #'fun-call
     ;; Extract the module path and symbol for the function at compile time.
     (define-values (mod-path fun-sym) (id->mod-path+sym #'fun))
     (with-syntax ([mod-path-datum (datum->syntax stx mod-path)]
                   [fun-sym-datum (datum->syntax stx fun-sym)])
       #'(if #false
             ;; Check the syntax according to `define2`, but do not call (or even evaluate) the
             ;; arguments. This also requires `fun` to be provided/required to avoid an unknown
             ;; identifier error.
             ;; It's important that we use a syntax id here (not a list) so that error reporting
             ;; within DrRacket is at the call site, not in the macro.
             fun-call
             ;; Collect the evaluated arguments with the resolved module path.
             (make-remote-call 'mod-path-datum
                               'fun-sym-datum
                               (list (~? (cons 'kw kw-arg)) ...)
                               (list (~? arg2) ...))))]))
