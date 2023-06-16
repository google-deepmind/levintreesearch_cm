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

(require syntax/parse/define
         (for-syntax racket/base)
         racket/fixnum
         racket/flonum
         racket/format
         racket/list
         racket/pretty
         racket/port
         safe-case
         text-table
         text-table/utils
         define2)

(provide (all-defined-out))

(module+ test (require rackunit))

(define-syntax assert
  (syntax-parser
    [(_ expr:expr)
     (syntax/loc #'expr
       (unless expr
           (error "Assertion failed:" 'expr)))]
    [(_ expr:expr ctx:expr ...)
     (syntax/loc #'expr
       (unless expr
         (eprintf "Assertion failed: ~v\n" 'expr)
         (pretty-write (ids->assoc ctx ...))
         (error "Assertion failed.")))]))

;; Suppresses any output on the output port (but not on the error port).
(define-syntax-rule (silent body ...)
  (parameterize ([current-output-port (open-output-nowhere)]) body ...))

(define (ask-enter)
  (display "\nPress Enter to continue")
  (flush-output)
  (void (read-line)))

; ----------

;; pack
(define-syntax-rule (ids->assoc id ...)
  (list (cons 'id id) ...))

(define (assoc-ref assoc sym)
  (cdr (or (assq sym assoc)
           (error "Symbol not found in assoc:" sym assoc))))

;; unpack
(define-syntax-rule (define-assoc (id ...) assoc)
  (begin
    (define id (assoc-ref assoc 'id))
    ...))

; ----------

(define-syntax-rule (++ x)
  (set! x (+ x 1)))

(define-syntax-rule (-- x)
  (set! x (- x 1)))

(define-syntax-rule (+= x n)
  (set! x (+ x n)))

(define-syntax-rule (cons! l v)
  (set! l (cons v l)))

; ----------

(define (list-of-lists->vector-of-fxvectors ll)
  (apply vector (map (λ (l) (apply fxvector l)) ll)))

(define-safe-case vector-kind-case (any fl fx))
;; Spreads the elements of l into n-vectors. Returns a vector of Xvectors,
;; where the vector kind of Xvector depends on `vector-kind` (`any` means `vector`).
;; All vectors have the same size ±1 (the first vectors are the longest ones).
;; Note that appending the vectors into a list does *not* restore the original order.
;;
;; Useful with `for/async` where n-vectors is the number of futures.
(define (list->subvectors l n-vectors #:? [vector-kind 'any])
  (define-values (vmake vset!)
    (vector-kind-case
     vector-kind
     [(any) (values make-vector vector-set!)]
     [(fl) (values make-flvector flvector-set!)]
     [(fx) (values make-fxvector fxvector-set!)]))

  (define len (length l))
  (define-values (q r) (quotient/remainder len n-vectors))
  (define res (build-vector (min len n-vectors) (λ (i) (vmake (+ q (if (< i r) 1 0))))))
  (for ([x (in-list l)]
        [i (in-naturals)])
    (define-values (qi ri) (quotient/remainder i n-vectors))
    (vset! (vector-ref res ri) qi x))
  res)

(module+ test
  (check-equal? (list->subvectors (range 100) 7)
                #(#(0 7 14 21 28 35 42 49 56 63 70 77 84 91 98)
                  #(1 8 15 22 29 36 43 50 57 64 71 78 85 92 99)
                  #(2 9 16 23 30 37 44 51 58 65 72 79 86 93)
                  #(3 10 17 24 31 38 45 52 59 66 73 80 87 94)
                  #(4 11 18 25 32 39 46 53 60 67 74 81 88 95)
                  #(5 12 19 26 33 40 47 54 61 68 75 82 89 96)
                  #(6 13 20 27 34 41 48 55 62 69 76 83 90 97)))
  (check-equal? (list->subvectors (map fx->fl (range 4)) 7 #:vector-kind 'fl)
                (vector (flvector 0.0)
                        (flvector 1.0)
                        (flvector 2.0)
                        (flvector 3.0))))

(define (path-string->string pth)
  (cond [(string? pth) pth]
        [(path? pth) (path->string pth)]
        [else (error "not a path-string" pth)]))

;; path are not written in a readable way, which has caused me several headaches.
;; using `build-path-string` consistently solves the problem.
(define (build-path-string . args)
  (path->string (apply build-path args)))

;==================;
;=== Text table ===;
;==================;

;; Useful to print a table of values from a sequence of ids
(define-syntax-parse-rule (print-ids-table [id:id ...] other-args ...)
  (print-simple-table (list '(id ...)
                            (list id ...))
                      other-args ...))
#| Example:
> (print-ids-table [x y])
x y
3 aaaa
|#

(define (list-stats-table #:? [header? #t] . name.l-s)
  (define (list->stats l)
    (define n (length l))
    (define ∑ (apply + l))
    (if (= n 0)
        (list ∑ +nan.0 +nan.0 +nan.0 n)
        (list ∑ (/ ∑ n 1.) (apply max l) (apply min l) n)))
  (define tab (map (λ (name+l) (cons (car name+l) (list->stats (cdr name+l))))
                   name.l-s))
  (if header?
      (cons '(Name Total Avg Max Min Count)
            tab)
      tab))

(define (print-list-stats . name.l-s)
  (print-simple-table
   #:->string (list (~r* #:precision '(= 2)) '... ~a)
   #:align '(left right ...)
   (apply list-stats-table name.l-s)))

#; ; Example:
(print-list-stats
 '("times" . (10 2 4.4 3 3 12 1250 3))
 '("expansions" . (30 12 22.0 24 15 108 8750 0 1 2 3)))
#|
Name         Total    Avg     Max  Min Count
times      1287.40 160.93 1250.00 2.00     8
expansions 8967.00 815.18 8750.00 0.00    11
|#
