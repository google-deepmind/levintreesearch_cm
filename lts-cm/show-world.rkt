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

(require racket/class
         racket/gui/dynamic
         pict
         define2)

(provide show-world)

;; on-key          : key-event? -> pict
;; pict-init       : pict?
;; parent          : (or/c (is-a?/c frame%) (is-a?/c dialog) #f)
;; label           : label-string?
;; width           : (or/c dimension-integer? #f)
;; height          : (or/c dimension-integer? #f)
;; frame-x         : (or/c position-integer? #f)
;; frame-y         : (or/c position-integer? #f)
;; frame-style     : (listof symbol?) ; see `style` in `frame`
;; x-scroll-amount : positive-real?
;; y-scroll-amount : positive-real?
(define (show-world #:? [on-key (λ (ev) #f)]
                    #:? [pict-init (blank 100 100)]
                    #:? [parent #f]
                    #:? [label "MrPict"]
                    #:? [width  #f]
                    #:? [height #f]
                    #:? [frame-x #f]
                    #:? [frame-y #f]
                    #:? [frame-style '()]
                    #:? [x-scroll-amount 100.]
                    #:? [y-scroll-amount 100.])
  (define the-pict pict-init)
  (define pict-drawer (make-pict-drawer the-pict))
  (define no-redraw? #f)
  (define pw (inexact->exact (ceiling (pict-width  the-pict))))
  (define ph (inexact->exact (ceiling (pict-height the-pict))))
  (define hscroll? (and width  (> pw width)))
  (define vscroll? (and height (> ph height)))

  (define pict-frame%
    (class (gui-dynamic-require 'frame%)
      (define/public (set-pict! p)
        (set! the-pict p)
        (set! pict-drawer (make-pict-drawer the-pict))
        (set! pw (inexact->exact (ceiling (pict-width  the-pict))))
        (set! ph (inexact->exact (ceiling (pict-height the-pict))))
        (set! no-redraw? #t)
        (send cv min-width  pw)
        (send cv min-height ph)
        (send fr resize pw ph)
        (set! no-redraw? #f)
        (send fr reflow-container)
        (send cv refresh))
      (super-new)))

  (define pict-canvas%
    (class (gui-dynamic-require 'canvas%)
      (inherit get-dc)

      (define/override (on-paint)
        (unless no-redraw?
          (let ([dc (get-dc)])
            (send dc clear)
            (let* ([xo (if (and width
                                (pw . < . width))
                           (- (/ width 2) (/ pw 2))
                           0)]
                   [yo (if (and height
                                (ph . < . height))
                           (- (/ height 2) (/ ph 2))
                           0)])
              (pict-drawer dc xo yo)))))

      (define/override (on-char ev)
        (define new-pict (on-key ev))
        (when (pict? new-pict)
          (send fr set-pict! new-pict)))

      (super-new [style (append (if hscroll? '(hscroll) '())
                                (if vscroll? '(vscroll) '()))]
                 [min-width  pw]
                 [min-height ph])
      (when (or hscroll? vscroll?)
        (send this init-auto-scrollbars pw ph 0. 0.))))

  (define fr (new pict-frame%
                  [parent parent]
                  [label label]
                  [style frame-style]
                  [x frame-x]
                  [y frame-y]))
  (define cv (new pict-canvas% [parent fr]))
  (send (send cv get-dc) set-smoothing 'aligned)

  ;; Centering either (but not both) seem to lose the other coordinate :/
  (cond [(and frame-x frame-y) (void)]
        [frame-x (send fr center 'vertical)]
        [frame-y (send fr center 'horizontal)]
        [else    (send fr center 'both)])
  (send fr show #t)
  (send cv focus)
  fr)

;;; Example
(module+ drracket
  (define size 100)
  (define colors
    #("black" "red" "green" "blue" "cyan" "yellow" "orange" "purple" "pink" "magenta" "gray"))
  (define color-idx 0)
  (define (make-pict)
    (disk size #:color (vector-ref colors color-idx)))

  ;; Press the arrow keys to change the size or color of the disk
  (show-world #:pict-init (disk size)
              #:on-key (λ (ev)
                         (case (send ev get-key-code)
                           [(up) (set! size (min 1000 (+ size 10)))
                                 (make-pict)]
                           [(down) (set! size (max 10 (- size 10)))
                                   (make-pict)]
                           [(right) (set! color-idx (modulo (+ color-idx 1) (vector-length colors)))
                                    (make-pict)]
                           [(left) (set! color-idx (modulo (- color-idx 1) (vector-length colors)))
                                   (make-pict)]
                           [else #f]))))
