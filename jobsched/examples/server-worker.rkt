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
         define2)

;;; run me simply with:
;;;   racket server-worker.rkt
;;; or
;;;   racket server-worker.rkt --n-workers 10
;;;
;;; The server and the worker are defined in the same file, but this is not necessary.

;==============;
;=== Worker ===;
;==============;

(module+ worker

  (define (run-job jb)
    ;; The worker has received a job node
    ;; Parse the data
    (match-define (list 'color color 'index i)
      (job-data jb))
    ;; Do some work.
    ;; Workers are allowed to write something to the output (println, etc.),
    ;; but it's generally a bad idea. Note that the worker's output is redirected
    ;; to its error port, so it arrives on the error port of the server rather than
    ;; on the input port (which is used for communicating data).
    (sleep 3)
    ;; Return a value to the server
    (if (= 0 (random 2))
      'FAILED
      'DONE))

  ;; Start the worker, which will be waiting for jobs
  (start-worker run-job))

;==============;
;=== Server ===;
;==============;

(module+ main
  (require global)

  (define-global:natural1 *n-workers* (processor-count) "number of workers to use in parallel")

  ;; This is the command to start a worker.
  ;; Creates a new Racket instance.
  (define (make-worker-command _worker-index)
    (make-racket-cmd "server-worker.rkt" #:submod 'worker))

  ;; Parse command-line arguments
  (void (globals->command-line)) ; for *jobsched-verb* and *n-workers*

  ;; Create a schedule, and tell it how to start a worker
  (define sched (make-scheduler make-worker-command))

  ;; Schedule some jobs
  (for* ([color (in-list '(red green blue))]
         [i 4])
    ;; The data must be `read`able
    (scheduler-add-job! sched #:data (list 'color color 'index i)))

  ;; Start 4 workers in parallel in different OS processes,
  ;; and ask them to process all the jobs.
  (scheduler-start sched
                   (*n-workers*)
                   #:before-start
                   (λ (sched jb)
                     (printf "About to start job ~a.\n" (job-data jb)))
                   #:after-stop
                   (λ (sched jb result)
                     (writeln (list 'result:   result
                                    'for-job: (job-data jb)
                                    (if (eq? result 'FAILED) 'Restarting. 'Done.)))
                     (when (eq? result 'FAILED)
                       ;; Adds the job back into the queue to try again.
                       (scheduler-add-job! sched #:data (job-data jb)))))

  (displayln "All jobs done. Exiting."))
