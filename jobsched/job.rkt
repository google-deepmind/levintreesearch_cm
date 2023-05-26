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

(require define2)

(provide (all-defined-out))

;; index: index of the job in creation order.
;; cost: used to order jobs in the priority queue.
;; data: user-specified. Must be serializable (read/write).
;; start-ms: milliseconds when the job starts running (as seen by the server)
;; stop-ms: milliseconds when the job stops running (as seen by the server)
(struct job (index cost data [start-ms #:mutable] [stop-ms #:mutable])
  #:transparent)
