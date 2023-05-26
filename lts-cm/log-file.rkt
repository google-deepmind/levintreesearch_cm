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

(require racket/fasl
         racket/fixnum
         racket/future
         racket/port
         timev
         "future.rkt"
         "misc.rkt"
         define2)

(provide (all-defined-out))

;; This reads a single .fasl log file.
(define (fasl-log-file->act+ctxs-seq log-file)
  (with-handlers ([exn:fail? (λ (e) #f)]) ; /!\ This is time consuming!
    (call-with-input-file* log-file (λ (in) (fasl->s-exp in #:datum-intern? #f)))))

;; Unfortunately, fasl doesn't support fxvectors, so we need to convert
;; to a list (could also be a vector though)
(define (write-act+ctxs-seq-to-fasl-log-file act-seq ctxs-seq log-file)
  (define dat
    (vector (apply fxvector act-seq)
            (apply vector ctxs-seq)))
  (call-with-output-file*
     #:exists 'replace
     log-file
     (λ (out) (s-exp->fasl dat out))))

(define (log-dir->log-files log-dir #:? [build? #f])
  (for/list ([f (in-list (directory-list log-dir #:build? #f))]
             #:when (regexp-match #px"^(\\d+)\\.rktd$" (path-string->string f)))
    (if build? (build-path log-dir f) f)))

(define (log-dir->fasl-files log-dir #:? [build? #f])
  (for/list ([f (in-list (directory-list log-dir #:build? #f))]
             #:when (regexp-match #px"^(\\d+)\\.fasl$" (path-string->string f)))
    (if build? (build-path log-dir f) f)))

;; Clean a learning directory, remove all task-dependent log files
(define (delete-log+fasl-files log-dir)
  (for ([f (in-list (log-dir->log-files log-dir #:build? #t))])
    (delete-file f))
  (for ([f (in-list (log-dir->fasl-files log-dir #:build? #t))])
    (delete-file f)))

;; This is faster than using `fasl-log-file->act+ctxs-seq` for each .fasl file in `log-dir`.
(define (log-dir->act+ctxs-seqs log-dir)
  ;; We use a hash to avoid collisions with futures
  (define files (log-dir->fasl-files log-dir #:build? #t))
  (define n-files (length files))

  (define bytess
    (timev
     "read bytes"
     (for/vector #:length n-files ([filename (in-list files)])
       (call-with-input-file filename port->bytes))))

  (define-values (starts ends) (partition-range (vector-length bytess) (processor-count)))

  (define res (make-vector n-files))
  (timev
   "convert bytes to vectors"
   (for/async ([start (in-list starts)]
               [end   (in-list ends)])
     (for ([bs (in-vector bytess start)] [idx (in-range start end)])
       (define dat (fasl->s-exp bs #:datum-intern? #f))
       (vector-set! res idx dat))))
  res)
