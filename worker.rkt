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
         global
         jobsched/worker
         jobsched/job
         racket/dict
         racket/file
         timev
         "cdb.rkt"
         "log-file.rkt"
         "misc.rkt"
         define2)

(provide (all-defined-out))

(define-global *log-dir* #false
  '("Directory where to read/write .rktd and .fasl files."
    "If set to #false, files are not written.")
  values
  values)

(define-global:boolean *save-rktd?* #true ; servers rely on this default, don't change it
  '("Whether to save the .rktd log files. Requires --log-dir."
    "The .rktd files are used in particular for visualizing trajectories."))

(define-global:boolean *save-fasl?* #true ; servers rely on this default, don't change it
  '("Whether to save the .fasl log files. Requires --log-dir."
    "The .fasl files are necessary for optimization when running a server."))

(define-global *cdb* #false
  "File containing the cdb to test on the argument to --problems"
  file-exists?
  values)

(define-global *problems* #false
  "The .rktd file containing the list of problems"
  file-exists?
  values)

(define-global:boolean *visited-hash?* #true
  "Use a hash for already-visited nodes? (transposition table)")

(define-global:natural1 *budget* #false
  "Search budget (number of expansions)")

;======================;
;=== Solver wrapper ===;
;======================;

(define (worker-solve-task
         #:! spec->state
         #:! solver
         #:! problems-file
         #:! line-num
         #:! visited-hash?
         #:? cdb
         #:? [budget #f]
         #:? [log-dir #f]
         #:? [save-log?  (and log-dir #t)]
         #:? [save-fasl? (and log-dir #t)]
         #:? [problem #f])
  (define spec
    (timev "get-problem" (or problem (list-ref (file->value problems-file) line-num))))
  (writeln spec)
  (define domain-state (spec->state spec))

  (define res (timev "solve"
                     (solver domain-state
                             #:cdb cdb
                             #:visited-hash? visited-hash?
                             #:budget budget)))

  (when (and log-dir
             (or save-log? save-fasl?)
             (eq? (dict-ref res 'status) 'SOLVED))
    (set! res (list* (cons 'problems-file problems-file) (cons 'problem-num line-num) res))
    (make-directory* log-dir)

    ;; Save contexts and actions to a separate file for fast loading.
    ;; This time we write them in forward order.
    (when save-fasl?
      (define ctxs-seq (dict-ref res 'ctxs-seq))
      (define act-seq  (dict-ref res 'act-seq))
      (timev "write-fasl"
             (write-act+ctxs-seq-to-fasl-log-file
              act-seq
              ctxs-seq
              (build-path log-dir (format "~a.fasl" line-num)))))

    (when save-log?
      (define log-file (build-path log-dir (format "~a.rktd" line-num)))
      ;; remove the contexts which can take a lot of space.
      ;; They can be found in the fasl file anyway.
      (timev "write-log"
             (write-to-file (dict-remove res 'ctxs-seq) log-file #:exists 'replace))))
    res)

;============;
;=== Main ===;
;============;

(define (domain-main #:! spec->state #:! solver)

  ;; Globals that are specific to a standalone main call

  (define-global:boolean *short-res?* #f "Return a shortened result dictionary")

  (define-global:natural0 *problem-line* #false
    "Line number (starting at 0) of a problem in the problem list.")

  (void (globals->command-line))

  (print-as-expression #f)

  (define cdb
    (if (*cdb*)
        (timev "load-cdb" (load-cdb (*cdb*)))
        no-value))

  (cond [(*problem-line*)
         ; run on an individual problem
         (define res
           (worker-solve-task #:spec->state spec->state
                              #:solver solver
                              #:cdb cdb
                              #:log-dir (*log-dir*)
                              #:problems-file (*problems*)
                              #:visited-hash? (*visited-hash?*)
                              #:line-num (*problem-line*)
                              #:budget (*budget*)
                              #:save-log? (*save-rktd?*)
                              #:save-fasl? (*save-fasl?*)))
         (if (*short-res?*)
             (dict-remove res 'ctxs-seq)
             res)]
        [else (displayln "--problem-line not specificied.")]))

(define (domain-worker #:! spec->state #:! solver)

  (void (globals->command-line))

  (print-as-expression #f)

  (define problems
    (let ([probs (file->value (*problems*))])
      (if (list? probs)
          (apply vector probs)
          probs))) ; assumes vector

  (define cdb
    (if (*cdb*)
        (load-cdb (*cdb*))
        no-value))

  (define (run-task jb)
    (define data (job-data jb))
    (define line   (dict-ref data 'line))
    (define budget (dict-ref data 'budget))
    (define res
      ;; Suppress the standard output. (The error port is untouched though.)
      (silent
       (worker-solve-task #:spec->state spec->state
                          #:solver solver
                          #:cdb cdb
                          #:budget budget
                          #:problem (vector-ref problems line)
                          #:log-dir (*log-dir*)
                          #:problems-file (*problems*)
                          #:visited-hash? (*visited-hash?*)
                          #:line-num line
                          #:save-log? (*save-rktd?*)
                          #:save-fasl? (*save-fasl?*))))
    ;; The number of contexts can be very large, so it can take long to transmit
    ;; and saturate the server.
    (dict-remove res 'ctxs-seq))

  (start-worker run-task))
