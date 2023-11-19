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
         racket/match
         "flonum.rkt"
         "misc.rkt"
         define2)

(provide (all-defined-out))

;**************************************************************************************;
;****                            Context Database (CDB)                            ****;
;**************************************************************************************;

;==================;
;=== Read/write ===;
;==================;

(define cdb-file-name "cdb-learn.fasl")

;; Prefab so as to be serializable
(struct CDB (ctx.idx-vec n-rows n-cols βmatrix) #:prefab)

(define (make-cdb n-cols)
  (CDB #f 0 n-cols (make-flvector 0)))

;;; Using fasl with its input/output port argument is by
;;; far the fastest way to read/write.
(define (save-cdb cdb file #:? [exists 'replace])
  (call-with-output-file* file (λ (out) (s-exp->fasl cdb out #:keep-mutable? #t)) #:exists exists))

(define (load-cdb file)
  (define cdb (call-with-input-file* file (λ (in) (fasl->s-exp in #:datum-intern? #f))))
  (match-define (CDB ctx.idx-vec n-rows n-cols βmatrix) cdb)
  (assert (vector? ctx.idx-vec) ctx.idx-vec)
  (assert (and (integer? n-rows) (> n-rows 0)) n-rows)
  (assert (and (integer? n-cols) (> n-cols 0)) n-cols)
  (assert (flvector? βmatrix))
  cdb)
