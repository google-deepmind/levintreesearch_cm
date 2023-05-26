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
         lts-cm/gui
         racket/runtime-path
         "sliding-tile.rkt"
         "pict.rkt"
         define2)

(provide play-stp main)

(define-runtime-path here (build-path 'same))

(define-values
  [play-stp main]
  (make-play-domain+main #:here here
                         #:label "Sliding Tile Puzzle"
                         #:->pict stp->pict
                         #:do-action do-action
                         #:action-codes '(up left down right)
                         #:solution? stp-solution?
                         #:spec->state list->stp))

(module+ main (main))
