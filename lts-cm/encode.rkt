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

(require (for-syntax racket/base
                     syntax/parse)
         "misc.rkt"
         racket/fixnum
         define2)

(provide (all-defined-out))

;***************************************************************************************;
;****                           Integer Encoding/Decoding                           ****;
;***************************************************************************************;

;; Returns a list of integers ints such that (integer-merge ints sizes) = n
;; remainder : (or/c #t #f 'check-0 'cons)
;;   If #f, the remainder is discared
;;   If #t, the remainder is returned as a second value
;;   If 'check-0, an exception is raised if the remainder is not 0.
;;   if 'cons, the remainder is cons'ed into the resulting list.
(define (fixnum->naturals n-orig sizes #:? [remainder 'check-0])
  (for/fold ([n n-orig]
             [res '()]
             #:result
             (case remainder
               [(check-0) (unless (= 0 n)
                            (error 'naturals->fixnum
                                   "Non-zero remainder. n-orig: ~a sizes: ~a remainder: ~a res: ~a"
                                   n-orig sizes n res))
                          res]
               [(#false) res]
               [(cons) (cons n res)]
               [else (values res n)]))
            ([size (in-list (reverse sizes))])
    (define-values (q r) (quotient/remainder n size))
    (values q (cons r res))))

;; Returns a integer encoding ints with sizes, similar
;; to the digits of a written number, but with a varying base (size).
;; Encoding can be performed sequentially by passing the previous code as the
;; `n` argument. See examples below.
;;
;; Can be used to decode seconds to yr:hr:mn:sec :
;; > (naturals->fixnum 1652558273 '(365 24 60 60) #:remainder #t)
;; '(146 19 57 53)
;; 52
;; where the remainder is the year relative to 1970,
;; and 146 is the 146th day of the year.
;; This does not account for leap years and such though.
(define (naturals->fixnum ints sizes [n 0])
  (for/fold ([n n])
            ([i (in-list ints)]
             [s (in-list sizes)])
    (assert (fx< -1 i s) ints sizes i s)
    (natural-encode1 i s n)))

;; Encode a sequence in forward order, with pairs [int size], maybe starting from a given number.
(define-syntax (naturals->fixnum* stx)
  (syntax-parse stx
    [(_ [v:expr size:expr] rst ...)
     #'(naturals->fixnum* 0 [v size] rst ...)]
    [(_ enc:expr [v:expr size:expr] rst ...)
     #'(naturals->fixnum* (natural-encode1 v size enc) rst ...)]
    [(_ x:expr) #'x]))

(define (naturals->fixnum/check ints sizes [n 0])
  ;; Also fails if ints and sizes don't have the same size
  (naturals->fixnum ints sizes n))

(define-syntax-rule (natural-encode1 int size n)
  (begin
    (assert (fx< -1 int size) int size)
    (fx+ (fx* n size) int)))

(module+ drracket
  (begin ;let ()

    (naturals->fixnum #;ints '(0 2 1 2 2 0 1 0) #;sizes '(2 3 4 5 6 7 8 9))
    ; 143145
    (naturals->fixnum* (0 2) (2 3) (1 4) (2 5) (2 6) (0 7) (1 8) (0 9))
    (fixnum->naturals #;code 143145 #;sizes '(2 3 4 5 6 7 8 9))
    ; '(0 2 1 2 2 0 1 0)

    ;; The above is equivalent to the below (hierarchical en/decoding):

    (define code
      (naturals->fixnum #;ints '(0 1 0) #;sizes '(7 8 9)
                      #;on-top-of
                      (naturals->fixnum #;ints '(0 2 1 2 2) #;sizes '(2 3 4 5 6))))
    ; 143145
    (define-values (dec1 next-code)
      (fixnum->naturals #;code 143145 #;sizes '(7 8 9) #:remainder #t))
    ; '(0 1 0) 284
    (fixnum->naturals next-code #;size '(2 3 4 5 6))
    ; '(0 2 1 2 2)
    ))

;; Example of hierarchical encoding/decoding.
;; You may need to run it several times to see a long enough list.
(module+ drracket
  (let ()
    ;; Encode
    ;; Note that ints and sizes should be read from right to left
    (define code
      (let loop ([size 3] [ints '()] [sizes '()])
        (define x (random size))
        (case x
          [(0) (loop 2 (cons x ints) (cons size sizes))]
          [(1) (loop 3 (cons x ints) (cons size sizes))]
          [(2)
           (displayln (list 'ints ints 'sizes sizes))
           (naturals->fixnum (cons 2 ints) (cons size sizes))])))
    ;; Decode
    (let loop ([code code] [size 3] [ints '()] [sizes '()])
      (define-values (lx next-code) (fixnum->naturals code (list size) #:remainder #t))
      (define x (car lx))
      (displayln (list lx next-code))
      (cond
        [(= 2 x)
         (displayln (list 'ints ints 'sizes sizes))]
        [else
         (loop next-code (if (= 0 x) 2 3) (cons x ints) (cons size sizes))]))))

(module+ test
  (require rackunit)
  (check-equal? (naturals->fixnum* [3 4]) 3)
  (check-equal? (naturals->fixnum #;ints '(0 2 1 2 2 0 1 0) #;sizes '(2 3 4 5 6 7 8 9))
                143145)
  (check-equal? (naturals->fixnum* (0 2) (2 3) (1 4) (2 5) (2 6) (0 7) (1 8) (0 9))
                143145)
  (check-equal? (fixnum->naturals #;code 143145 #;sizes '(2 3 4 5 6 7 8 9))
                '(0 2 1 2 2 0 1 0))

  (for ([i 10])
    (define sizes (build-list 10 (λ _ (+ 1 (random 10)))))
    (define ints (map random sizes))
    (check-equal? (fixnum->naturals (naturals->fixnum ints sizes) sizes)
                  ints))

  (check-exn exn:fail? (λ () (fixnum->naturals 10 '(7))))
  (check-equal? (fixnum->naturals 10 '(7) #:remainder #f) '(3))
  (check-equal? (call-with-values (λ () (fixnum->naturals 38 '(7) #:remainder #t)) list)
                '((3) 5))

  ;; For consistency with binary decomposition,
  ;; the lhs bits are the heaviest ones.
  ;; However, this is counter-intuitive for hierarchical decoding :(
  ;; Maybe it would be better to focus on hierarchical en/decoding.
  ;; (although this would also slow down encoding by a `reverse` operation)
  (check-equal? (fixnum->naturals 23 '(2 2 2 2 2))
                '(1 0 1 1 1))
  (check-equal? (fixnum->naturals 23 '(2 2 2 2 2 2))
                '(0 1 0 1 1 1))
  (check-equal? (fixnum->naturals 23 '(2 2 2 2 2 2 2))
                '(0 0 1 0 1 1 1))


  (check-equal? (naturals->fixnum '(0 2 1) '(2 3 4) 5589)
                134145)
  (check-equal? (call-with-values (λ () (fixnum->naturals 134145 '(2 3 4) #:remainder #t))
                                  list)
                '((0 2 1) 5589))

  (check-equal? (fixnum->naturals 143145 '(2 3 4 5 6 7 8 9))
                '(0 2 1 2 2 0 1 0))
  ;; We can decompose the decoding if we start from the end
  (check-equal? (call-with-values (λ () (fixnum->naturals 143145 '(7 8 9) #:remainder #t))
                                  list)
                '((0 1 0) 284))
  (check-equal? (call-with-values (λ () (fixnum->naturals 284 '(2 3 4 5 6) #:remainder #t))
                                  list)
                '((0 2 1 2 2) 0)))

;; Encode all the values of the bytes `bts` found at the fxvector `idxs` of indices
;; within a single number.
;; The values found must not exceed `max-value` otherwise the encoding may be incorrect.
(define (bytes-context/encode bts idxs #:! max-value)
  (define N (+ max-value 1))
  (for/fold ([enc 0])
            ([idx (in-fxvector idxs)])
    (define v (bytes-ref bts idx))
    (naturals->fixnum* enc [v N])))
