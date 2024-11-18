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

(require global
         racket/path
         define2)

(provide (all-defined-out))

(define-global:boolean *jobsched-verb?* #false
  '("Display information from the jobsched?"))

(define-syntax-rule (when-verb body ...)
  (when (*jobsched-verb?*)
    (display "jobsched: ")
    body ...))

(define ask-ready-message 'JOBSCHED:WORKER-READY?)
(define ready-message     'JOBSCHED:READY)


;; From:
;; https://github.com/racket/racket/blob/master/pkgs/racket-benchmarks/tests/
;;   racket/benchmarks/places/place-processes.rkt#L63
(define (current-executable-path)
  (parameterize ([current-directory (find-system-path 'orig-dir)])
    (find-executable-path (find-system-path 'exec-file) #f)))

(define (current-collects-path)
  (let ([p (find-system-path 'collects-dir)])
    (if (complete-path? p)
      p
      (path->complete-path p (or (path-only (current-executable-path))
                                 (find-system-path 'orig-dir))))))

;; Notice: `#f` are filtered out of `args`.
(define (make-racket-cmd path-to-prog #:? [submod #f] #:? [errortrace? #f] . args)
  (define path-to-prog-str
    (if (string? path-to-prog)
      path-to-prog
      (path->string path-to-prog)))
  (let ([args (filter values args)])
    (if submod
        `(,(current-executable-path) ;"/usr/bin/env" "racket"
         "-X" ,(current-collects-path)
         "-l" "racket/init"
         ,@(if errortrace? '("-l" "errortrace") '())
         "-e" ,(format "(require (submod (file ~s) ~a))" path-to-prog-str submod)
         "--"
         ,@args)
        `(,(current-executable-path) "-X" ,(current-collects-path)
                                     ,@(if errortrace? '("-l" "errortrace" "-t-") '())
                                     ,path-to-prog ,@args))))

;; Maybe we should use `fasl` here to speed up the transfer, but
;; whether it's advantageous should be checked.
(define (send-msg v [out (current-output-port)])
  ; Make sure the data we send out is self-delimiting (list, vector, struct, ...)
  ; but not symbols or numbers) by encapsulating it into a list.
  (write (list v) out)
  (flush-output out))

(define (receive-msg [in (current-input-port)])
  (define obj (read in))
  (if (pair? obj)
    (car obj) ; decapsulate
    obj)) ; could be eof
