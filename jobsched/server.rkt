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

(require (for-syntax racket/base syntax/parse)
         data/heap
         racket/list
         racket/system
         racket/struct
         (only-in racket/future processor-count)
         "utils.rkt"
         "job.rkt"
         define2)

(provide (struct-out scheduler)
         make-scheduler
         scheduler-n-queued-jobs
         scheduler-add-job!
         scheduler-start
         processor-count
         make-server
         server-start
         server-close
         start-simple-server)

#|
To monitor cpu speed and temperature:
watch -n 3 "cat /proc/cpuinfo  | grep MHz; sensors"
|#

(define-syntax-rule (++ x)
  (set! x (+ x 1)))

(define-syntax-rule (-- x)
  (set! x (- x 1)))

;—————————————————————————————————————————————————————————————————————————————————————————————————————

(struct scheduler
  (queue
   [K #:mutable]
   make-worker-command
   [workers #:mutable]
   [worker-idx #:mutable]
   [n-active-jobs #:mutable])
  #:transparent)

(define (make-scheduler make-worker)
  (define queue (make-heap (λ (a b)
                             (define ca (job-cost a))
                             (define cb (job-cost b))
                             (if (= ca cb)
                               (<= (job-index a) (job-index b))
                               (<= ca cb)))))
  (scheduler queue #;K: 0 make-worker #;workers: '() #;worker-idx -1 #;n-active-jobs: 0))

(define (scheduler-n-workers sched)
  (length (scheduler-workers sched)))

(define (scheduler-next-job-index! g)
  (define K+1 (+ 1 (scheduler-K g)))
  (set-scheduler-K! g K+1)
  K+1)

(define (scheduler-n-queued-jobs g)
  (heap-count (scheduler-queue g)))

;; Returns a `job` or #f if the scheduler is empty.
(define (scheduler-min g)
  (and (> (scheduler-n-queued-jobs g) 0)
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

;—————————————————————————————————————————————————————————————————————————————————————————————————————

(struct worker (index cmd in out err pid handler start-ms [job #:mutable] [state #:mutable])
  #:property prop:evt
  (λ (self) (wrap-evt (worker-in self) (λ _ self)))
  #:transparent)

(define-syntax-rule (define-states state ...)
  (begin (define state 'state) ...))

(define-states
  state:starting
  state:ready?-sent
  state:ready
  state:running
  state:killed
  state:closing
  state:closed)

;; Runs the command line cmd in the OS, and return the `worker`.
(define (start-worker name cmd)
  (define err (current-error-port))
  (define-values (in out pid _err handler)
    (apply values (apply process*/ports #f #f err cmd)))
  (worker name cmd in out err pid handler (current-milliseconds) #;job: #f state:starting))

(define (worker-ask-ready wk)
  (send-msg message:ask-ready (worker-out wk))
  (set-worker-state! wk state:ready?-sent))

(define (worker-close wk)
  (when-verb (printf "Closing worker: ~a\n" wk))
  (check-state-in wk state:ready state:starting)
  (define in  (worker-in  wk))
  (define out (worker-out wk))
  (send-msg message:close-worker out)
  (set-worker-state! wk state:closing)
  ;; Returns the thread, that can be sync'ed with.
  #;
  (thread
   ;; Wait until the port of the worker is closed.
   (λ ()
     (println "worker is closing...")
     (sleep 1)
     (writeln "Hey!!" out) ; this should fail with port closed?
     ;; We need to wait for the process to be closed
     (sync (port-closed-evt out) (port-closed-evt in))
     (println "worker has closed")
     (set-worker-state! wk state:closed))))

(define (worker-kill wk)
  (when-verb (printf "Terminating worker: ~a\n" wk))
  ((worker-handler wk) 'kill)
  (set-worker-state! wk state:killed)
  (define in  (worker-in wk))
  (define out (worker-out wk))
  (when in  (close-input-port in))
  (when out (close-output-port out))
  (worker-close wk))

(define-syntax (check-state-in stx)
  (syntax-parse stx
    [(_ wk:expr state:expr ...+)
     (with-syntax ([orig-stx stx])
       #'(unless (memq (worker-state wk) (list state ...))
           (raise-syntax-error
            #f
            (format "worker state must be one of ~v\n got: ~v" (list state ...) (worker-state wk))
            #'orig-stx #;#'wk)))]))

#; ; Example error:
(begin
  (define wk (worker 'name 'cmd 'in 'out 'err 'pid 'handler 1 #f state:starting))
  (check-state-in wk state:ready))

;—————————————————————————————————————————————————————————————————————————————————————————————————————

;; before-start : scheduler? job? -> any
;;   Callback called right before a job is started.
;; after-stop : scheduler? job? status result -> any
;;   status : (or/c 'halted 'timeout)
;;   result : any/c
;;   Callback called right after a job has finished.
(define (scheduler-start sched [n-workers (scheduler-n-workers sched)]
                         #:before-start [before-start void]
                         #:after-stop [after-stop void]
                         #:close-workers? [close? #true])
  (define start-seconds (current-seconds))

  ;; Check no active job
  (unless (= 0 (scheduler-n-active-jobs sched))
    (error 'scheduler-start
           "expected: (= 0 (scheduler-n-active-jobs sched))"
           (scheduler-n-active-jobs sched)))

  ;;; Helpers
  (define (+=n-active-jobs n)
    (set-scheduler-n-active-jobs! sched (+ n (scheduler-n-active-jobs sched))))
  ;;
  (define (get-workers)
    (scheduler-workers sched))
  ;;
  (define (set-workers! wks)
    (set-scheduler-workers! sched wks))
  ;;
  (define (new-worker!)
    (define worker-idx (+ 1 (scheduler-worker-idx sched)))
    (set-scheduler-worker-idx! sched worker-idx)
    (define wk (start-worker worker-idx ((scheduler-make-worker-command sched) worker-idx)))
    (set-workers! (cons wk (get-workers))))
  ;;
  (define (close-one-worker!)
    (define wks (get-workers))
    (unless (empty? wks)
      (worker-kill (first wks))
      (set-workers! (rest wks))))
  
  (when (and (= 0 n-workers)
             (> (scheduler-n-queued-jobs sched) 0))
    (raise-argument-error 'scheduler-start
                          "n-workers = 0 but jobs are enqueued."
                          n-workers))

  ;; Match the number of workers asked by the user
  (begin
    ;; The user asks to change the number of workers.
    (define n-existing-workers (scheduler-n-workers sched))
    ;; First, remove all unnecessary workers
    (for ([i (in-range n-workers n-existing-workers)]) (close-one-worker!))
    ;; Finally, top up the number of workers if necessary.
    ;; (yes, n-existing-workers is fine here)
    (for ([i (in-range n-existing-workers n-workers)]) (new-worker!)))

  ;; For all already-running workers, we ask them to reply to a signal
  ;; so they will match the `sync` below.
  ;; If a worker is starting, it will send a ready message when ready.
  ;; If a worker is already ready, we ask it to send a new ready message.
  ;; (We could actually bypass this sync/ack and just send them jobs directly,
  ;; but this is simple).
  (for ([wk (in-list (get-workers))])
    (check-state-in wk state:ready state:starting)
    (when (eq? (worker-state wk) state:ready)
      (worker-ask-ready wk)))

  (unless (= 0 (scheduler-n-queued-jobs sched))
    (let loop ()
      ;; Find a worker that has output a value.
      ;; The workers must start by sending out `ready-message`.
      (define wk (apply sync (get-workers)))
      (define res (receive-msg (worker-in wk)))
      (define now (- (current-seconds) start-seconds))
      (define wk-ready?
        (cond [(eof-object? res)
               (when-verb (printf "time: ~a; NOTICE: Worker ~a terminated. Starting a new one.\n"
                                  now (worker-index wk)))
               (worker-kill wk)
               ;; push the job back into the queue (should we make a child node instead?)
               (define jb (worker-job wk))
               (when jb
                 (+=n-active-jobs -1)
                 (scheduler-add-job! sched #:data (job-data jb) #:cost (job-cost jb)))
               ;; Remove the worker and add a new one.
               (set-workers! (remove wk (get-workers)))
               (new-worker!)
               #false]
            
              [(eq? res message:ready)
               (when-verb (printf "time: ~a; WORKER READY\n" now))
               (check-state-in wk state:ready?-sent state:starting)
               (set-worker-state! wk state:ready)
               #true]
            
              [(worker-job wk)
               (check-state-in wk state:running)
               ; Processing job result
               (+=n-active-jobs -1)
               (define jb (worker-job wk))
               (set-job-stop-ms! jb (current-milliseconds))
               (when-verb
                (printf (string-append
                         "time: ~as; worker: ~a; job: ~a stopped; cost: ~a; took: ~ams\n")
                        now (worker-index wk) (job-index jb)  (job-cost jb)
                        (- (job-stop-ms jb) (job-start-ms jb))))
               (after-stop sched jb res) ; callback
               (set-worker-job! wk #f)
               (set-worker-state! wk state:ready)
               #true]
            
              [else
               ;; A worker has sent a message, but the job is #f. This means that a worker sends a
               ;; message before `start-worker` is able to deal with the worker's outputs.
               (raise-argument-error 'scheduler-start "A worker message" res)]))

      ;; Send a new job to the worker if it is ready
      (when wk-ready?
        (check-state-in wk state:ready)
        (define jb (scheduler-extract-min! sched))
        (when jb
          ;; Give a chance to modify the data in the node, and maybe add sibling nodes
          (before-start sched jb) ; callback
          (set-worker-job! wk jb)
          (+=n-active-jobs 1)
          (set-job-start-ms! jb (current-milliseconds))
          (set-worker-state! wk state:running)
          (send-msg (struct->list jb) (worker-out wk))))

      (unless (= 0 (scheduler-n-active-jobs sched)) ; also implies queue is empty
        (loop))))

  (when close? (scheduler-close sched)))

  ;; Terminate all workers, close the ports, etc.
(define (scheduler-close sched)
  (when-verb (printf "Waiting for all ~a workers to close\n" (scheduler-n-workers sched)))
  (for-each worker-close (scheduler-workers sched))
  (for ([wk (in-list (scheduler-workers sched))])
    ((worker-handler wk) 'wait))
  (when-verb (println "All workers closed."))
  (set-scheduler-workers! sched '()))

;==============;
;=== Server ===;
;==============;

(define (make-server #:! worker-file
                     #:? [submod-name 'worker]
                     #:? [n-workers #f])
  (define (make-worker-command _worker-index)
    (make-racket-cmd worker-file #:submod submod-name))
  (define sched (make-scheduler make-worker-command))
  (when n-workers
    ;; Start the workers
    (server-start sched #:data-list '() #:process-result (λ (data result) (void))
                  #:n-workers n-workers
                  #:close-workers? #false))
  sched)

;; Workers are NOT closed on exit by default.
(define (server-start sched
                      #:! data-list
                      #:! process-result
                      #:? [n-workers (scheduler-n-workers sched)]
                      #:? [close-workers? #false])
  (for ([data (in-list data-list)])
    (scheduler-add-job! sched #:data data))
  (scheduler-start sched
                   n-workers
                   #:after-stop (λ (sched jb result) (process-result (job-data jb) result))
                   #:close-workers? close-workers?))

;; Closes all the workers gracefully.
(define (server-close sched)
  (scheduler-close sched))

;; A simpler server that hides the scheduler and the jobs
;; All workers are closed on exit
(define (start-simple-server #:! worker-file
                             #:! data-list
                             #:! process-result
                             #:? [submod-name 'worker]
                             ;; n-proc kept for bwd compat. The default value MUST be for n-proc
                             ;; and not for n-workers.
                             #:? [n-proc (min (length data-list) (processor-count))]
                             #:? [n-workers n-proc])
  (define sched (make-server #:worker-file worker-file #:submod-name submod-name))
  (server-start sched
                #:data-list data-list
                #:process-result process-result
                #:n-workers n-workers)
  (server-close sched))
