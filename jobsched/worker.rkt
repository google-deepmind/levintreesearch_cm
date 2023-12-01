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
         "utils.rkt"
         "job.rkt"
         define2)

(provide
 (contract-out
  [start-worker
   (->* [(procedure-arity-includes/c 1)]
        (#:silent? any/c)
        any)]))

;; run-job : gbs-node? -> any/c
;; The result of `start-worker` must be writeable and readable.
(define (start-worker run-job #:? [silent? #f])
  (send-msg ready-message) ; This is important

  (let loop ()
    ;; If it's more than a few ms, something's wrong (IO on the server side?)
    (define msg (receive-msg))
    (unless (eof-object? msg)
      (define nd (apply job msg))

      ;; The custodian ensures that all open files are closed. I was getting a
      ;; "too many files opened" error after 30min and many simple jobs,
      ;; but frankly I don't understand why yet.
      (define cust (make-custodian))
      (define res
        (if silent?
            (parameterize ([current-custodian cust]
                           ;; The output port is used for communication with the server,
                           ;; and must thus be reserved for that, so we temporarily redirect the
                           ;; output port to the error port
                           [current-output-port (open-output-nowhere)])
              (run-job nd))
            (parameterize ([current-custodian cust]
                           ;; The output port is used for communication with the server,
                           ;; and must thus be reserved for that, so we temporarily redirect the
                           ;; output port to the error port
                           [current-output-port (current-error-port)])
              (run-job nd))))
      (custodian-shutdown-all cust)

      (send-msg res)
      (loop))))

