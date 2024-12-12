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

(require racket/file
         racket/list
         racket/runtime-path
         (for-syntax racket/base)
         lts-cm/optimize
         lts-cm/policy
         lts-cm/server
         lts-cm/server-chain
         lts-cm/misc
         "rubiks-cube.rkt"
         define2)

(provide generate-train-file)

(define-runtime-path program-path "rubiks-cube.rkt")
(define log-dir-name "rubiks-cube")

;; Training dataset files are generated on demand to reduce
;; the size of the github repo.
;; Fixed random generator (picked randomly) for reproducibility,
;; but different from the one used in the paper (which is lost, but
;; the files still live on my computer if necessary).
(define rand-gen
  (vector->pseudo-random-generator
   #(3980949144 784629000 4187860733 2858366043 3537181480 1167541959)))
(define data-dir (make-temporary-directory "lts-cm-temp~a"))

(define ((generate-train-file n-cubes scrambles-min scrambles-max #:? [data-dir data-dir]) iter)
  (define f (build-path-string data-dir
                               (format "cube~a-walk~a-~a.rktd" iter scrambles-min scrambles-max)))
  (printf "Generating training set. scrambles ∈ [~a, ~a]\n" scrambles-min scrambles-max)
  (define bts
    (parameterize ([current-pseudo-random-generator rand-gen])
      (for/list ([i (in-range n-cubes)])
        (define n (random scrambles-min (+ scrambles-max 1)))
        (cube-bytes (make-random-cube n)))))
  (write-to-file bts f)
  f)

(module+ main

  ;; Number of generated cube per dataset.
  ;; This can be reduced to 50000 which saves training time,
  ;; but is at the expense of larger solution lengths.
  (define n-cubes 100000)

  (define train-files
    (append
     (list (generate-train-file n-cubes 1 5)
           (generate-train-file n-cubes 5 10)
           (generate-train-file n-cubes 10 15)
           (generate-train-file n-cubes 15 20)
           (generate-train-file n-cubes 20 25)
           (generate-train-file n-cubes 25 30)
           (generate-train-file n-cubes 35 40)
           (generate-train-file n-cubes 45 50)) ; 800 000 cubes
     (make-list 90 (generate-train-file n-cubes 50 50))))
  (define test-files '("data/cube-test-1000.rktd"))

  ;; These default globals can be overwritten from the command line.
  (*ε-mix* 0.001)
  (*ε-low* 0.0001)
  (*regularizer* 10.)
  (*gap-factor* 0.5)
  (*optim-steps* 200)
  (*budget-init* 21000)
  (*budget-test* 1000000)

  ;; TODO: Delete the data-dir after the chain?

  (server-chain-main
   #:n-actions n-actions
   #:log-dir-name log-dir-name
   #:program-path program-path
   #:train-files train-files
   #:test-files test-files
   ;; We don't test on the first few training iterations because it takes far too long for
   ;; no interesting result.
   #:test-delay 3))
