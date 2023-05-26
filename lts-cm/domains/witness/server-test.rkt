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

(require racket/runtime-path
         (for-syntax racket/base)
         lts-cm/optimize
         lts-cm/server
         lts-cm/policy
         lts-cm/server-chain
         (only-in "witness.rkt" n-actions)
         define2)

;; This is useful to quickly test out whether the whole process works

(define-runtime-path program-path "witness.rkt")
(define log-dir-name "witness")

(module+ main

  (define train-files
    '("data/puzzles_1000_test.rktd"
      "data/puzzles_1000_test.rktd")) ; make sure the chain works
  (define test-files
    '("data/puzzles_1000_test.rktd"))

  ;; These default globals can be overwritten from the command line,
  ;; which is parsed at each call of `run!`
  ;; But *problems*, *test-cdb*, *init-cdb* must not be touched".
  (*ε-mix* 0.001)
  (*ε-low* 0.0001)
  (*regularizer* 10.)
  (*gap-factor* 0.5)
  (*optim-steps* 200)
  (*budget-init* 2000)
  (*budget-test* 5000000)

  (server-chain-main
   #:n-actions n-actions
   #:log-dir-name log-dir-name
   #:program-path program-path
   #:train-files train-files
   #:test-files test-files))

