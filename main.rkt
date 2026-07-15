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
         define2)

(provide *timev?*
         timev-apply
         timev)

(define-global:boolean *timev?* #true
  '("Whether timev is verbose"))

(define time-verb-indent (make-parameter 0))

(define (timev-apply label #:? [disp? (*timev?*)] proc . args)
  (cond [disp?
         (define-values (res cpu real gc)
           (parameterize ([time-verb-indent (+ 1 (time-verb-indent))])
             (time-apply proc args)))
         (printf "~a~a cpu: ~a real: ~a gc: ~a\n"
                 (make-string (time-verb-indent) #\space)
                 label cpu real gc)
         (apply values res)]
        [else
         (apply proc args)]))

(define-syntax timev
  (syntax-rules ()
    [(_ label #:disp? expr body ...)
     (timev-apply label #:disp? expr (λ () body ...))]
    [(_ label body ...)
     (timev-apply label (λ () body ...))]))

(module+ test
  (require rackunit
           racket/port
           racket/string)
  (check
   string-prefix?
   (with-output-to-string (λ () (timev "abc" 'a)))
   "abc cpu: ")
  (check-equal?
   (with-output-to-string (λ () (with-globals ([*timev?* #f]) (timev "abc" 'a))))
   "")
  (check-equal?
   (with-output-to-string (λ () (timev "abc" #:disp? #f 'a)))
   "")
  (check
   string-prefix?
   (with-output-to-string (λ () (with-globals ([*timev?* #f]) (timev "abc" #:disp? #t 'a))))
   "abc cpu: "))
