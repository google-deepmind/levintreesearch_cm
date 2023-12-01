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

(require data/heap
         racket/system
         racket/struct
         (only-in racket/future processor-count)
         "utils.rkt"
         "job.rkt"
         define2)

(provide (struct-out scheduler)
         make-scheduler
         scheduler-count
         scheduler-add-job!
         scheduler-start
         processor-count
         start-simple-server)

#|
To monitor cpu speed and temperature:
watch -n 3 "cat /proc/cpuinfo  | grep MHz; sensors"
|#

(define-syntax-rule (++ x)
  (set! x (+ x 1)))

(define-syntax-rule (-- x)
  (set! x (- x 1)))


(struct scheduler (queue [K #:mutable] make-worker-command [workers #:mutable])
  #:transparent)

(define (make-scheduler make-worker)
  (define queue (make-heap (λ (a b)
                             (define ca (job-cost a))
                             (define cb (job-cost b))
                             (if (= ca cb)
                               (<= (job-index a) (job-index b))
                               (<= ca cb)))))
  (scheduler queue 0 make-worker '()))

(define (scheduler-next-job-index! g)
  (define K+1 (+ 1 (scheduler-K g)))
  (set-scheduler-K! g K+1)
  K+1)

(define (scheduler-count g)
  (heap-count (scheduler-queue g)))

;; Returns a `job` or #f if the scheduler is empty.
(define (scheduler-min g)
  (and (> (scheduler-count g) 0)
       (heap-min (scheduler-queue g))))

;; Returns a `job` and removes the node from the scheduler,
;; or #f if the scheduler is empty.
(define (scheduler-extract-min! g)
  (define m (scheduler-min g))
  (when m
    (heap-remove-min! (scheduler-queue g)))
  m)

(define (scheduler-add-job! g #:! data #:? [cost 0])
  (define jb (job (scheduler-next-job-index! g)
                   cost
                   data
                   0 0))
  (heap-add! (scheduler-queue g) jb))

(struct worker (index cmd in out err pid handler start-ms [job #:mutable])
  #:property prop:evt
  (λ (self) (wrap-evt (worker-in self) (λ _ self)))
  #:transparent)

;; Runs the command line cmd in the OS, and return the `worker`.
(define (start-worker name cmd)
  (define err (current-error-port))
  (define-values (in out pid _err handler)
    (apply values (apply process*/ports #f #f err cmd)))
  (worker name cmd in out err pid handler (current-milliseconds) #f))

(define (worker-close wk)
  (when-verb (printf "Closing worker: ~a\n" wk))
  (define in (worker-in wk))
  (define out (worker-out wk))
  (when in  (close-input-port in))
  (when out (close-output-port out)))

(define (worker-terminate wk)
  (when-verb (printf "Terminating worker: ~a\n" wk))
  ((worker-handler wk) 'kill)
  (worker-close wk))


;; before-start : scheduler? job? -> any
;;   Callback called right before a job is started.
;; after-stop : scheduler? job? status result -> any
;;   status : (or/c 'halted 'timeout)
;;   result : any/c
;;   Callback called right after a job has finished.
(define (scheduler-start sched n-workers
                         #:before-start [before-start void]
                         #:after-stop [after-stop void])
  (define start-seconds (current-seconds))

  (define worker-idx -1)
  (define (new-worker)
    (++ worker-idx)
    (start-worker worker-idx ((scheduler-make-worker-command sched) worker-idx)))
  (define workers
    (build-list n-workers (λ _ (new-worker))))

  (define n-pending 0) ; number of pending jobs

  (let loop ()
    ;; Find a worker that has output a value.
    ;; The workers must start by sending out `ready-message`.
    (define wk (apply sync workers))
    (define res (receive-msg (worker-in wk)))
    (define now (- (current-seconds) start-seconds))
    (cond [(eof-object? res)
           (when-verb (printf "time: ~a; NOTICE: Worker ~a terminated. Starting a new one.\n"
                              now (worker-index wk)))
           (worker-terminate wk)
           ;; push the job back into the queue (should we make a child node instead?)
           (define jb (worker-job wk))
           (when jb
             (-- n-pending)
             (scheduler-add-job! sched #:data (job-data jb) #:cost (job-cost jb)))
           ;; Remove the worker and add a new one.
           (set! workers (cons (new-worker) (remove wk workers)))]
          [(eq? res ready-message)
           (when-verb (printf "time: ~a; WORKER READY\n" now))
           (void)]
          [(worker-job wk)
           ; Processing job result
           (-- n-pending)
           (define jb (worker-job wk))
           (set-job-stop-ms! jb (current-milliseconds))
           (when-verb
            (printf (string-append
                     "time: ~as; worker: ~a; job: ~a stopped; cost: ~a; took: ~ams\n")
                    now (worker-index wk) (job-index jb)  (job-cost jb)
                    (- (job-stop-ms jb) (job-start-ms jb))))
           (after-stop sched jb res) ; callback
           (set-worker-job! wk #f)]
          [else
           ; The job is #f, which means that a worker sends a message before `start-worker`
           ; is able to deal with the worker's outputs.
           (when-verb
             (printf "WARNING: received unprocessed message (before `start-worker`?): ~v\n"
                     res))])

    ;; Send a new job to the worker
    (define jb (scheduler-extract-min! sched))
    (when jb
      ;; Give a chance to modify the data in the node, and maybe add sibling nodes
      (before-start sched jb) ; callback
      (set-worker-job! wk jb)
      (++ n-pending)
      (set-job-start-ms! jb (current-milliseconds))
      (send-msg (struct->list jb) (worker-out wk)))

    (unless (= 0 n-pending)
      (loop)))

  ;; Terminate all workers, close the ports, etc.
  (for-each worker-terminate workers))

;; A simpler server that hides the scheduler and the jobs
(define (start-simple-server #:! worker-file
                             #:! data-list
                             #:! process-result
                             #:? [submod-name 'worker]
                             #:? [n-proc (min (length data-list) (processor-count))])
  (define (make-worker-command _worker-index)
    (make-racket-cmd worker-file #:submod submod-name))
  (define sched (make-scheduler make-worker-command))
  (for ([data (in-list data-list)])
    (scheduler-add-job! sched #:data data))
  (scheduler-start sched
                   n-proc
                   #:after-stop (λ (sched jb result) (process-result (job-data jb) result))))
