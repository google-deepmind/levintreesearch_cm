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

(require racket/date
         racket/format
         racket/match
         racket/string
         "misc.rkt"
         "encode.rkt"
         define2)

(provide (all-defined-out))


;; This should be somewhere else, like in misc, but it depends on "encode.rkt". Could be in misc2.rkt?
(define (seconds->time-string t
                              #:? [leading-zeros? #t]
                              #:? [hours? #t]
                              #:? [minutes? hours?]
                              #:? [seconds? minutes?]
                              #:? [total-seconds? #t])
  (match-define (list d h m s) (fixnum->naturals t '(24 60 60) #:remainder 'cons))
  (string-join
   (filter values
           (list (and (or leading-zeros? (> d 0))
                      (format "~ad" d))
                 (and hours?
                      (or leading-zeros? (> d 0) (> h 0))
                      (format "~ah" h))
                 (and minutes?
                      (or leading-zeros? (> d 0) (> h 0) (> m 0))
                      (format "~am" m))
                 (and seconds?
                      (format "~as" s))
                 (and total-seconds? (format "(~as)" t))))
   " "))


;; An iso date string that's compatible with filepaths on various OSes
(define (date-iso-file)
  (define (fmt x [len 2])
    (~r x #:min-width len #:pad-string "0"))
  (match (current-date)
    [(date s m h d month y week-day year-day dst? time-zone-offset)
     (format "~a-~a-~a--~a-~a-~a"
             (fmt y 4) (fmt month) (fmt d) (fmt h) (fmt m) (fmt s))]))
