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

(require racket/contract
         racket/port
         racket/tcp
         "utils.rkt"
         "job.rkt"
         define2)

(provide
 (contract-out
  [start-worker
   (->* [(procedure-arity-includes/c 1)]
        [#:silent? any/c]
        any)]
  [start-simple-worker
   (->* [(procedure-arity-includes/c 1)]
        [#:silent? any/c]
        any)]))

;; run-job : job? -> any/c
;; The result of `start-worker` must be writeable and readable.
(define (start-worker run-job #:? [silent? #f])
  (define tcp-port-str (getenv "JOBSCHED_PORT"))
  (unless tcp-port-str
    (error 'start-worker
           "Missing JOBSCHED_PORT environment variable. Workers must be spawned by the jobsched server."))
  (define tcp-port (string->number tcp-port-str))
  (define-values (tcp-in tcp-out) (tcp-connect "127.0.0.1" tcp-port))

  (send-msg message:ready tcp-out) ; This is important

  (let loop ()
    (define msg (receive-msg tcp-in))
    (cond
      [(eof-object? msg)
       ;; Terminate the worker.
       (void)]
      [(eq? msg message:close-worker)
       ;; Exit gracefully.
       (close-output-port tcp-out)
       (close-input-port tcp-in)
       (void)]
      [(eq? msg message:ask-ready)
       (send-msg message:ready tcp-out)
       (loop)]
      [else
       (define jb (apply job msg))

       ;; The custodian ensures that all open files are closed. I was getting a
       ;; "too many files opened" error after 30min and many simple jobs,
       ;; but frankly I don't understand why yet.
       (define cust (make-custodian))
       (define res
         (parameterize ([current-custodian cust]
                        ;; When silent, redirect output to nowhere.
                        ;; Otherwise, leave output alone — it goes to the subprocess's
                        ;; stdout/stderr and doesn't interfere with the TCP protocol.
                        [current-output-port (if silent?
                                                 (open-output-nowhere)
                                                 (current-output-port))])
           (run-job jb)))
       (custodian-shutdown-all cust)

       (send-msg res tcp-out)
       (loop)])))

(define (start-simple-worker run #:? [silent? #f])
  (start-worker (λ (jb) (run (job-data jb))) #:silent? silent?))

