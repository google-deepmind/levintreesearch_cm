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

(require racket/dict
         racket/file
         racket/math
         racket/match
         racket/string
         racket/format
         (except-in jobsched when-verb)
         global
         text-table
         text-block
         timev
         "cdb.rkt"
         "date.rkt"
         "log-file.rkt"
         "policy.rkt"
         "optimize.rkt"
         "misc.rkt"
         "verbose.rkt"
         define2)

(provide server-main
         *init-cdb*
         *test-cdb*
         *problems*
         *optim-steps*
         *visited-hash?*
         *budget-init*
         *budget-max*
         *n-workers*
         *regularizer*)

(*verbose?* #false)
(*timev?* #false) ; don't display timev info by default (can still be changed on command line)

(define-global *init-cdb* #f
  '("File containing the initial cdb. A copy will be made in log-dir."
    "If #f, an empty cdb is created instead.")
  (λ (x) (or (not x) (file-exists? x)))
  values)

(define-global *test-cdb* #f
  '("File containing the initial cdb to test. A copy will be made in log-dir."
    "Sets init-cdb to test-cdb, and sets optim-steps to 0. Hence the cdb"
    "will not be optimized as problems are solved, and the budget is "
    "unconditionally increased after each solving iteration."
    "NOTICE: Currently all problems are re-attempted at each bootstrap iteration,"
    "including the already solved ones. Pick a high enough init-budget to reduce"
    "or remove this effect.")
  (λ (x) (or (not x) (file-exists? x)))
  values)

(define-global *problems* #f
  ".rktd file containing the list of problems."
  file-exists?
  values)

(define-global:natural0 *optim-steps* 1000
  "Number of optimization steps to perform")

(define-global:boolean *visited-hash?* #t
  '("Use a hash table of the visited states? (aka transposition tables)"))

(define-global:natural1 *budget-init* 125 "Initial budget in number of expansions")
(define-global:real *budget-factor* 2 "Budget factor")
(define-global:natural1 *budget-max* (expt 10 20) "Maximum budget to use") ; virtually infinite
(define-global:natural1 *n-workers* (exact-ceiling (/ (processor-count) 2))
  (format "Number of processors to use (available: ~a)" (processor-count)))
(define-global:real *regularizer* 0. "Regularizer factor" '("--reg"))

;; Returns the set of already-solved problems by checking for the presence of the fasl file.
(define (log-dir->solved-hash log-dir)
  (for/hasheq ([f (in-list (log-dir->fasl-files log-dir #:build? #f))])
    (values (string->number (path->string (path-replace-extension f #"")))
            #t)))

;; Run the optimizer
(define (optimize log-dir register! #:! reg #:! n-actions)
  (register! 'start-optimizer (current-seconds))
  (update-for-server #:register register!
                     #:log-dir log-dir
                     #:Tmax (*optim-steps*)
                     #:reg reg
                     #:n-actions n-actions)
  (register! 'stop-optimizer (current-seconds)))

;; Send all tasks to the workers and collect the results
(define (solve-server make-solver-worker
                      #:! tasks
                      #:! register!
                      #:! budget
                      #:! solved-hash
                      #:! n-workers
                      #:! train?)

  ;; The previous optimization step may have used a lot of memory. Let's free
  ;; what we can for the workers.
  (collect-garbage)

  (define sched (make-scheduler make-solver-worker))

  ;; Regularly check for free RAM and kill everything when less than 5% remains.
  ;; Workers will be killed due to input port being close.
  (define mem-thread (start-memory-guard-thread sched register!))

  (for ([task (in-list tasks)])
    (scheduler-add-job! sched  #:data (cons (cons 'budget budget) task)))

  (define n-tasks (scheduler-count sched))
  (printf "Number of tasks: ~a\n" n-tasks)

  (define start-seconds (current-seconds))
  (register! 'start-solver start-seconds)
  (register! 'budget budget)
  (register! 'n-problems n-tasks)

  (define n-solved 0)
  (define n-unsolved 0)
  (define n-no-solution 0) ; subset of n-unsolved
  (define n-new-solved 0)
  (define solved-expansions 0)
  (define new-solved-expansions 0) ; expansions of newly solved problems
  (define expansions 0)
  (define solved-time-ms-list '())
  (define solved-expansions-list '())
  (define solved-length-list '())
  (scheduler-start sched n-workers
                   #:before-start
                   (λ (sched jb) (void))
                   #:after-stop
                   (λ (sched jb result)
                     (define line (dict-ref (job-data jb) 'line))
                     (define search-status (dict-ref result 'status))
                     (define time-ms
                       ;; solving time:
                       (dict-ref result 'time-ms)
                       ;; solving time + replay + writing logs + comm w/ server:
                       #;(- (job-stop-ms jb) (job-start-ms jb)))
                     (when (*verbose?*)
                       (printf
                        "time: ~as; index: ~a; cost: ~a; line: ~a; status: ~a; took: ~ams\n"
                        (- (current-seconds) start-seconds)
                        (job-index jb)
                        (job-cost jb)
                        line
                        search-status
                        time-ms))
                     (case search-status
                       [(timeout)
                        (++ n-unsolved)]
                       [else
                        (define this-expansions (dict-ref result 'expansions))
                        (+= expansions this-expansions)
                        (case search-status
                          [(budget #false) ; #false means couldn't solve
                           (++ n-unsolved)
                           (unless search-status
                             (++ n-no-solution))]
                          [(SOLVED)
                           (++ n-solved)
                           (define len (dict-ref result 'len))
                           (cons! solved-time-ms-list time-ms)
                           (cons! solved-expansions-list this-expansions)
                           (cons! solved-length-list len)
                           (unless (hash-ref solved-hash line #f)
                             (++ n-new-solved)
                             (+= new-solved-expansions this-expansions))
                           (+= solved-expansions this-expansions)]
                          [else
                           (error "Unknown search status" search-status result)])])))
  (kill-thread mem-thread)
  ;; (We could group the write operations, but it's not worth it given the small amount overall)
  (register! 'stop-solver (current-seconds))
  ;; NOTICE: n-solved is the number of problems solved in the *latest* iteration,
  ;; while the number reported by the optimizer is the total number of solved problems, which
  ;; may be larger.
  (register! 'n-solved              n-solved)
  (register! 'n-unsolved            n-unsolved)
  (register! 'n-no-solution         n-no-solution) ; included in n-unsolved
  (register! 'n-new-solved          n-new-solved)
  (register! 'solved-expansions     solved-expansions)
  (register! 'expansions            expansions)
  (register! 'new-solved-expansions new-solved-expansions)
  (printf "Solver finished. #solved: ~a.\n" n-solved)
  (printf (if train? "\nTrain stats:\n" "\nTest stats:\n"))
  (define solve-stats
    (list-stats-table (cons "solved-time-ms"    solved-time-ms-list)
                      (cons "solved-expansions" solved-expansions-list)
                      (cons "solved-lengths"    solved-length-list)))
  (register! 'solve-stats solve-stats)
  (print-table solve-stats
               #:->string (λ (x) (if (and (rational? x) (inexact? x))
                                     (~r x #:precision '(= 3))
                                     (~a x)))
               #:row-sep? '(#t #f)
               #:align '(left right)
               #:framed? (not train?)))

;======================;
;=== Some utilities ===;
;======================;

(define (start-memory-guard-thread sched register!)
  (when (file-exists? "/proc/meminfo") ; unix/linux only
    (thread
     (λ ()
       (let loop ()
         (sleep 10)
         (match (string-split (file->string "/proc/meminfo"))
           [(list-rest "MemTotal:" totalkB "kB"
                       "MemFree:" freekB "kB"
                       "MemAvailable:" availkB "kB"
                       _rst)
            (when (< (string->number availkB) (* .05 (string->number totalkB)))
              (register! 'status 'out-of-memory)
              (eprintf "OUT OF MEMORY")
              (exit))]
           [else
            (eprintf "Warning: cannot read or parse /proc/meminfo")])
         (loop))))))

;============;
;=== Main ===;
;============;

(define (server-main #:! program-path #:! log-base-dir #:! n-actions
                     ;; callback to call once the log-dir is known
                     #:? [on-log-dir (λ (log-dir) (void))])

  ;; other-cmd-args are given to the workers
  (define other-cmd-args
    (globals->command-line #:mutex-groups (list (list *init-cdb* *test-cdb*))))

  (define init-cdb (or (*init-cdb*)
                       (*test-cdb*)))

  (define log-dir (build-path-string log-base-dir (date-iso-file)))
  (printf "log-dir: ~a\n" log-dir)

  (on-log-dir log-dir) ; callback

  (make-directory* log-dir)
  (define log-file (build-path-string log-dir "server-log.rktd"))

  (unless (and (*problems*) (file-exists? (*problems*)))
    (error "problems file does not exist. Given" (*problems*)))
  (define n-problems (length (file->value (*problems*))))

  (define cdb-file
    (cond [(*test-cdb*)] ; no copy, and we know the file exists
          [else
           (define f (build-path-string log-dir cdb-file-name))
           (make-parent-directory* f)
           (cond [(*init-cdb*)
                  (copy-file init-cdb f)]
                 [else
                  ;; Create an empty cdb
                  (save-cdb (make-cdb n-actions) f)])
           f]))

  (define (append-to-log-file! key val)
    (with-output-to-file log-file #:exists 'append
      (λ ()
        (writeln (cons key val)))))

  (append-to-log-file! 'ccla    (current-command-line-arguments))
  (append-to-log-file! 'globals (globals->assoc))
  (append-to-log-file! 'cdb     cdb-file) ; it's a string

  (define tasks
    (for/list ([i (in-range n-problems)])
      (list (cons 'line i))))

  (define (make-solver-worker name)
    (apply make-racket-cmd
           program-path
           ;#:errortrace? #t ;; Uncomment this line to add debugging info, after deleting .zos
           #:submod 'worker
           "--log-dir" log-dir
           ;; /!\ `#f` and not `""` as `#f` is filtered out (but not `""`)
           (if (*visited-hash?*) #f "--no-visited-hash") ; transposition tables
           ;; During training the log files (.rktd) are not saved,
           ;; while during testing the act+ctxs files (.fasl) are not saved.
           (if (*test-cdb*) "--no-save-fasl" "--no-save-rktd")
           "--cdb" cdb-file
           "--problems" (*problems*)
           "--ε-mix" (number->string (*ε-mix*))
           other-cmd-args))

  (define (make-register)
    (define result '())
    (case-lambda
      [() (reverse result)]
      [(key) (dict-ref result key)]
      [(key val)
       (append-to-log-file! key val)
       (set! result (cons (cons key val) result))]))

  (define n-tasks (length tasks))

  (define optimize? (and (> (*optim-steps*) 0)
                         (not (*test-cdb*))))

  (define start (current-seconds))
  ;; `max-solved` may be smaller than the total number of solved problems (as reported by
  ;; the optimizer).
  (let loop ([iter 1] [max-solved 0] [budget (*budget-init*)])
    (append-to-log-file! 'iteration iter)
    (append-to-log-file! 'reg (*regularizer*))
    (newline)
    (displayln (underline
                (format "Bootstrap iteration: ~a    previous solved: ~a/~a budget: ~a"
                        iter max-solved n-tasks budget)))
    (printf "\n** Starting solver. n-workers=~a\n" (*n-workers*))

    ;; Solving phase
    (define solver-register (make-register))
    (timev "\nSolver: " #:disp? #t
           (solve-server make-solver-worker
                         #:n-workers (*n-workers*)
                         #:tasks tasks
                         #:register! solver-register
                         #:budget budget
                         #:solved-hash (log-dir->solved-hash log-dir)
                         #:train? (not (*test-cdb*))))
    (define n-solved          (solver-register 'n-solved))
    (define n-new-solved      (solver-register 'n-new-solved))
    (define n-unsolved        (solver-register 'n-unsolved))
    (define n-no-solution     (solver-register 'n-no-solution))
    (define solved-expansions (solver-register 'solved-expansions))

    ;; Notice: Even if there are no newly solved problems,
    ;; it can be that the found trajectories are different (refactoring),
    ;; and thus optimization may still be necessary.
    (when (and optimize? (> n-solved 0))
      (printf "\n** Starting optimizer. n-proc-futures=~a\n" (*n-futures*))
      (define optim-register (make-register))
      (timev "\nOptimizer: " #:disp? #t
             (optimize log-dir optim-register #:reg (*regularizer*) #:n-actions n-actions)))

    (define new-budget
      (min (*budget-max*)
           (cond [(= 0 n-unsolved)
                  budget]

                 [(and optimize?
                       (>= n-solved 8)
                       (>= n-solved (* 1.25 max-solved)))
                  ;; We've made significant progress, possibly the budget is too high
                  (max (*budget-init*) (quotient budget 2))]

                 [else
                  ;; Adaptive budget (see paper).
                  ;; The purpose is to avoid losing computation time if search does not make
                  ;; enough progress.
                  (define α (*budget-factor*))
                  (+ (* α budget)
                     (quotient (* (- α 1) solved-expansions) n-unsolved))])))

    ;; Do another iteration of the loop if...
    (when (and (< n-no-solution n-unsolved) ; there are still some problems to solve, and
               (or (< budget (*budget-max*)) ; we haven't used the max budget yet, or
                   optimize?)) ; we can still optimize the policy.
      (loop (+ iter 1) (max max-solved n-solved) new-budget)))

  (printf "\nTotal time for this file: ~a\n"
          (seconds->time-string (- (current-seconds) start) #:total-seconds? #t))

  log-dir)
