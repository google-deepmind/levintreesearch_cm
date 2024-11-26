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

(require (for-syntax racket/base)
         racket/runtime-path
         lts-cm/optimize
         lts-cm/server
         lts-cm/policy
         lts-cm/server-chain
         lts-cm/misc
         "sliding-tile.rkt"
         define2)

(define-runtime-path program-path "sliding-tile.rkt")
(define log-dir-name "stp")

(module+ main
  (require global)

  (define-global:boolean *intro?* #t "Display the introduction text?")

  ;; These default globals can be overwritten from the command line.
  (*ε-mix* 0.001)
  (*ε-low* 0.0001)
  (*regularizer* 10.)
  (*gap-factor* 0.5)
  (*optim-steps* 200)
  (*budget-init* 7000)
  (*budget-test* 30000)

  (void (globals->command-line))
  (current-command-line-arguments #())

  (define train-files
    ;; For file ...-walkNN, the problems are generated with a random walk
    ;; backward from the solution state of NN steps
    '("data/stp5x5-train-1000-walk20.rktd"
      "data/stp5x5-train-1000-walk30.rktd"
      "data/stp5x5-train-1000-walk40.rktd"
      "data/stp5x5-train-1000-walk50.rktd"
      "data/stp5x5-train-1000-walk60.rktd"
      "data/stp5x5-train-1000-walk70.rktd"
      "data/stp5x5-train-1000-walk80.rktd"
      "data/stp5x5-train-1000-walk90.rktd"))
  (define test-files
    ;; The test set was generated separately from the training set
    '("data/stp5x5-test-1000-walk100.rktd"))

  ;; A small story to set the mood
  (when (*intro?*)
    (displayln "\nA 5x5 Sliding Tile test set of 1000 problems of medium difficulty...")
    (sleep 2)
    (displayln "Like this one:")
    (displayln
     (stp->string (list->stp '(2 7 3 8 4 5 1 13 0 9 6 11 16 18 14 20 12 17 15 23 21 10 22 24 19))))
    (sleep 3)
    (displayln "\nWithout training, only 25/1000 test problems are solved")
    (displayln "with a budget of 100 000 search steps.")
    (sleep 3)
    (displayln "\nCan training on simpler problems improve this number?\n")
    (sleep 2)
    (ask-enter))

  (let ()
   (server-chain-main
    #:n-actions n-actions
    #:log-dir-name log-dir-name
    #:program-path program-path
    #:train-files train-files
    #:test-files test-files)
    (void)))
