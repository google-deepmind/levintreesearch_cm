#lang racket
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

(require jobsched
         racket/runtime-path)

(define-runtime-path worker-file "adder-worker.rkt")

;; List of job data.
(define data-list
  (for/list ([num1 (in-range 10)] [num2 (in-range 10)])
    (list num1 num2)))

;; What to do once a result is received.
(define (process-result data result)
  (match data
    [(list num1 num2)
     (printf "~a + ~a = ~a\n" num1 num2 result)]))

;; Start the server and wait for the results.
(start-simple-server #:worker-file worker-file
                     #:data-list data-list
                     #:process-result process-result)
