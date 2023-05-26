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

(require racket/gui/base
         racket/class
         racket/list
         racket/file
         racket/dict
         racket/format
         racket/string
         racket/math
         pict
         "pict.rkt"
         "show-world.rkt"
         (only-in "worker.rkt" *log-dir*)
         global
         define2)

(provide (all-defined-out))

(define-global:real *replay-speed* 0.1 "Pause during replay of actions")

(define (play-world wld
                    #:! state->pict
                    #:! do-action
                    #:! key-event->action
                    #:? [user-actions-label #f]
                    #:? [label "World"]
                    #:? [act-seq '()]
                    #:? [pcond-seq '()]
                    #:? [solution? (λ (state) #f)])
  (set! act-seq   (for/vector ([x act-seq]) x)) ; make sure it's a vector
  (set! pcond-seq (for/vector ([x pcond-seq]) x))
  (define pcond-seq-len (vector-length pcond-seq))
  (define worlds (list wld))
  (define t 0)
  (define on-sequence? #t)
  (define thrd #f)

  (define (kill-thrd!)
    (when thrd
      (kill-thread thrd)
      (set! thrd #f)))

  (define header
    (vl-append
     (hbl-append (text "Auto:  ")
                 (text "Enter: " '(bold)) (text "run ")
                 (text "Space: " '(bold)) (text "step ")
                 (text "Backspace: " '(bold)) (text "undo ")
                 (text "Home: " '(bold)) (text "reset "))
     (if user-actions-label (text (string-append "User:  " user-actions-label)) (blank))))

  (define (make-pict)
    (define wld (first worlds))
    (hb-append
     (vl-append
      header
      (state->pict wld)
      (text (format "t: ~a" t))
      (if (solution? wld) (text "Solution found" '() 20) (blank))
      (if (and pcond-seq (< t pcond-seq-len) on-sequence?)
          (let ([p (vector-ref pcond-seq t)])
            (vl-append (rotate (make-pcond-rect p) (/ pi 2))
                       (text (format "1/p: ~a" (~r (/ 1. p) #:precision '(= 4))))))
          (blank)))
     ))

  (define (action! act)
    (define soko2 (do-action (first worlds) act))
    (set! worlds (cons soko2 worlds))
    (set! t (+ t 1))
    (make-pict))

  (define fr
    (show-world
     #:label label
     #:pict-init (make-pict)
     #:on-key
     (λ (ev)
       (define act (key-event->action ev))
       (cond
         [act
          (kill-thrd!)
          (set! on-sequence? #f)
          (action! act)]
         [else
          (define key (send ev get-key-code))
          (case key
            [(home)
             (kill-thrd!)
             (set! worlds (list (last worlds)))
             (set! on-sequence? #t)
             (set! t 0)
             (make-pict)]
            [(#\backspace) ; undo
             (kill-thrd!)
             (unless (empty? (rest worlds))
               (set! worlds (rest worlds))
               (set! t (- t 1))
               (when (= t 0) (set! on-sequence? #t))
               ;; We could also cache the picture, but this would take more memory.
               (make-pict))]
            [(#\space)
             (kill-thrd!)
             (when (and on-sequence? (< t (vector-length act-seq)))
               (action! (vector-ref act-seq t)))]
            [(#\return)
             (kill-thrd!)
             (when on-sequence?
               (set! thrd
                     (thread (λ ()
                               (let loop ()
                                 (when (< t (vector-length act-seq))
                                   (define new-pict (action! (vector-ref act-seq t)))
                                   (send fr set-pict! new-pict)
                                   (sleep/yield (*replay-speed*))
                                   (loop)))))))]
            [else #f])]))))
  fr)

(define (make-play-domain+main #:! here
                               #:! label
                               #:! ->pict
                               #:! do-action
                               #:! action-codes
                               #:! solution?
                               #:! spec->state)

  (define (play-domain state
                       #:? [label label]
                       #:? act-seq
                       #:? pcond-seq)
    (play-world state
                #:state->pict ->pict
                #:do-action do-action
                #:key-event->action
                (λ (ev) (index-of action-codes (send ev get-key-code)))
                #:user-actions-label (string-join (map ~a action-codes) " ")
                #:label label
                #:act-seq act-seq
                #:pcond-seq pcond-seq
                #:solution? solution?))

  (define (play-domain-log-file f)
    (displayln f)
    (define d (file->value f))
    (define problems-file (dict-ref d 'problems-file))
    (define problems (file->value (if (absolute-path? problems-file)
                                      problems-file
                                      (build-path here problems-file))))
    (define pb (list-ref problems (dict-ref d 'problem-num)))
    (play-domain (spec->state pb)
                 #:act-seq (dict-ref d 'act-seq)
                 #:pcond-seq (dict-ref d 'pcond-seq)))

  (define (play-domain-choose-from dir)
    (define f (get-file "Choose a log file"
                        #f ; parent
                        dir
                        #f ; file
                        #f ; extension
                        '() ; style
                        '(("lts-cm log" "*.rktd"))
                        ))
    (when f (play-domain-log-file f)))

  (define (main #:? [more-globals '()])
    (define-global *log* #f "log file" file-exists? values)
    (void (globals->command-line
           #:mutex-groups (list (list *log* *log-dir*))
           #:globals
           (append (list *log* *log-dir* *replay-speed*)
                   more-globals)))
    (cond [(*log*)
           (void (play-domain-log-file (*log*)))]
          [(directory-exists? (*log-dir*))
           (void (play-domain-choose-from (*log-dir*)))]
          [else
           (error "Directory does not exist" (*log-dir*))]))

  (values play-domain main))
