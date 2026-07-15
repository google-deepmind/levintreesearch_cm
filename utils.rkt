#lang racket/base
#| Copyright 2023 DeepMind Technologies Limited.

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

https://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.|#

(require (for-syntax racket/base syntax/parse)
         global
         racket/fasl
         racket/file
         racket/match
         racket/path
         racket/string
         syntax/location
         define2)

(provide (all-defined-out))

(define-global:boolean *jobsched-verb?* #false
  '("Display information from the jobsched?"))

(define-syntax-rule (when-verb body ...)
  (when (*jobsched-verb?*)
    (display "jobsched: ")
    body ...))

(define (syntax->path stx)
  (define dir (syntax-source-directory stx))
  (define name (syntax-source-file-name stx))
  (cond [(and dir name) (build-path dir name)]
        [name]
        [else #f]))

(define-syntax (this-file stx)
  ;; The source of #'stx object is the file where the syntax is *defined*,
  ;; while the source of #'id below is the file where the syntax is *used*.
  (syntax-case stx ()
    [(id) #'(syntax->path #'id)]))

;; mflatt suggests to use this instead of (this-file), though I'm not sure about
;; the pros/cons:
;;https://racket.discourse.group/t/how-to-dynamic-require-from-the-enclosing-module/3345/2?u=laurent.o
;; Using (this-file) has the benefit of the symmetry with the server's arguments.
;; Can also use (quote-module-path) for the current submodule.
(define-syntax-rule (top-module)
  (quote-module-path ".."))

(define message:ask-ready    'JOBSCHED:WORKER-READY?)
(define message:ready        'JOBSCHED:READY)
(define message:close-worker 'JOBSCHED:CLOSE-WORKER)

;; From:
;; https://github.com/racket/racket/blob/master/pkgs/racket-benchmarks/tests/
;;   racket/benchmarks/places/place-processes.rkt#L63
(define (current-executable-path)
  (parameterize ([current-directory (find-system-path 'orig-dir)])
    ;; WARNING: May not work properly when there are links to follow
    (find-executable-path (find-system-path 'exec-file) #f)))

;; Notice: `#f` are filtered out of `args`.
(define (make-racket-cmd path-to-prog #:? [submod #f] #:? [errortrace? #f] . args)
  (define path-to-prog-str
    (if (string? path-to-prog)
      path-to-prog
      (path->string path-to-prog)))
  (let ([args (filter values args)])
    `(,(current-executable-path)
      ,@(for*/list ([p (in-list (current-library-collection-paths))]
                    [x '("-X" #f)])
          (or x p))
      "-l" "racket/init"
      ,@(if errortrace? '("-l" "errortrace") '())
      ,@(if submod
            (list "-e" (format "(require (submod (file ~s) ~a))" path-to-prog-str submod))
            (list "-t" path-to-prog))
      "--"
         ,@args)))

;; Maybe we should use `fasl` here to speed up the transfer, but
;; whether it's advantageous should be checked.
(define (send-msg v [out (current-output-port)])
  (s-exp->fasl v out #:keep-mutable? #true) ; mutable hashes are faster than immutable ones
  (flush-output out))

(define (receive-msg [in (current-input-port)])
  (if (port-closed? in)
      eof
      (fasl->s-exp in #:datum-intern? #false)))


;; Monitors memory usage every wait-seconds.
;; When at most OOM-ratio memory remains, call on-OOM (which defaults to exiting immediately).
;; May not kill child processes instantly if called from a server.
;; Linux-specific (reads /proc/meminfo).
(define (start-memory-guard-thread #:? [on-OOM (λ ()
                                                 (eprintf "OUT OF MEMORY")
                                                 (exit))]
                                   #:? [OOM-ratio 0.05]
                                   #:? [wait-seconds 10]) ; wait between each query
  (and (file-exists? "/proc/meminfo") ; unix/linux only
       (thread
        (λ ()
          (let loop ()
            (sleep wait-seconds) ; every 10 seconds
            (match (string-split (file->string "/proc/meminfo"))
              [(list-rest "MemTotal:" totalkB "kB"
                          "MemFree:" freekB "kB"
                          "MemAvailable:" availkB "kB"
                          _rst)
               (when (< (string->number availkB) (* OOM-ratio (string->number totalkB)))
                 (on-OOM))]
              [else
               (eprintf "Warning: cannot read or parse /proc/meminfo")])
            (loop))))))
