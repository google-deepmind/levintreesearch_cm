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

(require jobsched)

;; This is the command to start a worker.
;; Creates a new Racket instance.
(define (make-worker-command _worker-index)
  (make-racket-cmd "adder-worker.rkt"))

;; Create a schedule, and tell it how to start a worker
(define sched (make-scheduler make-worker-command))

;; Schedule some jobs
(for* ([num1 (in-range 10)] [num2 (in-range 10)])
  (scheduler-add-job! sched #:data (list num1 num2)))

(define (process-result sched jb result)
  (match (job-data jb)
    [(list num1 num2)
     (printf "~a + ~a = ~a\n" num1 num2 result)]))

;; Start the server
(scheduler-start sched (processor-count) #:after-stop process-result)
