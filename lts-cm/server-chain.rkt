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

(require global
         racket/file
         racket/path
         text-block
         timev
         "cdb.rkt"
         "date.rkt"
         "log-file.rkt"
         "policy.rkt"
         "server.rkt"
         "misc.rkt"
         define2)

(provide (all-defined-out))

(define-global *log-base-dir* (build-path-string (find-system-path 'home-dir) "lts-cm-logs")
  "Directory where all the generated logs are stored."
  path-string?
  values)

(define-global:boolean *keep-all-cdbs?* #f
  "Keep all intermediate CDBs? (This may take quite some space on disk)")

(define-global:natural1 *budget-test* 5000000
  "Budget min and max to use during tests")

;; Notice: This emulates a command line and thus modifies some globals.
(define (server-chain-main
         #:! n-actions #:! program-path #:! log-dir-name #:! train-files #:! test-files
         #:? [test-delay 0])

  (define (run! #:? on-log-dir)
    (server-main
     #:n-actions n-actions
     #:log-base-dir chain-log-dir
     #:program-path program-path
     #:on-log-dir on-log-dir))

  (void (globals->command-line))
  ;; Remove any ccla to avoid conflicts with setting globals in subsequent calls to `server-main`
  (current-command-line-arguments #())

  (displayln
   (frame (superimpose (make-tblock 1 80) "Levin Tree Search with Context Models" 'center 'center)))

  (define chain-log-dir (build-path-string (*log-base-dir*) log-dir-name))
  (define program-dir (path-only program-path))

  (make-directory* chain-log-dir) ; ensure the directory exists

  (define chain-log-file
    (build-path-string chain-log-dir (string-append "chain-" (date-iso-file) ".rktd")))
  (printf "chain-log-file: ~a \n" chain-log-file)

  (define init-cdb (*init-cdb*))
  (when init-cdb
    (printf "cdb-file: ~a\n" init-cdb))

  (define n-train-files (length train-files))

  (define start (current-seconds))
  (define last-cdb-file
    (for/fold ([cdb-file init-cdb])
              ([train-file (in-list train-files)]
               [iter (in-naturals 1)])
      (let* ([train-file (if (procedure? train-file)
                            (timev "generate training file" (train-file iter))
                            train-file)]
             [train-file (path-string->string train-file)])
        (newline)
        (displayln (frame (format " Chain iteration: ~a/~a train-file: ~a "
                                  iter n-train-files train-file)
                          #:style 'double))

        ;; Solve and learn
        (*init-cdb* cdb-file)
        (*problems* (if (absolute-path? train-file)
                        train-file
                        (build-path-string program-dir train-file)))
        (define log-dir
          (run!
           #:on-log-dir
           (λ (log-dir)
             (write-to-file
              (list 'train (path-string->string log-dir) train-file) chain-log-file #:exists 'append)
             (display-to-file "\n" chain-log-file #:exists 'append))))

        ;; Once finished, delete all .log and .rktd files as they take a LOT of space!
        ;; Only server-log.rktd is kept.
        (timev "*** Deleting all training task log files to save space ***"
               (delete-log+fasl-files log-dir))
        ;; Also delete the cdb-file, unless it was given as input to the chain
        (unless (or (*keep-all-cdbs?*)
                    (equal? cdb-file init-cdb)) ; also takes care of init-cdb=#f
          (delete-file cdb-file))

        ;; This cdb-file now exists and is the optimized version after training
        ;; at this iteration
        (define new-cdb-file (build-path-string log-dir cdb-file-name))

        ;; Tests
        (when (> iter test-delay)
          (newline)
          (displayln (frame " Tests " #:style 'single))
          (for ([test-file (in-list test-files)])
            (newline)
            (displayln
             (underline
              (format "Chain iteration: ~a/~a [train-file: ~a] test-file: ~a"
                      iter n-train-files train-file test-file)
              #:style 'double))
            (define test-path (build-path-string program-dir test-file))
            (define test-log-dir #f)
            (with-globals ([*init-cdb* #f]
                           [*test-cdb* new-cdb-file]
                           [*ε-mix* 0.]
                           [*problems* test-path]
                           ;; a little ad-hoc
                           [*budget-init* (*budget-test*)]
                           [*budget-max*  (*budget-test*)])
              (run!
               #:on-log-dir
               (λ (log-dir)
                 (set! test-log-dir log-dir)
                 (write-to-file (list 'test (path-string->string log-dir) test-file)
                                chain-log-file
                                #:exists 'append)
                 (display-to-file "\n" chain-log-file #:exists 'append))))

            (printf "\nTo visualize some solutions:\nracket \"~a\" --log-dir \"~a\"\n"
                    (build-path-string program-dir "gui.rkt")
                    test-log-dir)))

        new-cdb-file)))

  (printf "\nTotal chain time: ~a\n"
          (seconds->time-string (- (current-seconds) start) #:total-seconds? #t))

  (values chain-log-file last-cdb-file))
