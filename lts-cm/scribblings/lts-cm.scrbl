#lang scribble/manual
@require[@for-label[lts-cm
                    racket/base]
         scribble/examples
         lts-cm/delta-secant]

@title{Levin Tree Search with Context Models}
@author{lorseau}

@(define the-eval (make-base-eval '(require racket/math lts-cm/delta-secant)))

@defmodule[lts-cm]

See the @hyperlink["https://github.com/deepmind/levintreesearch_cm#readme"]{README} for introductory
examples for the LTS+CM algorithm on several domains such as Rubik's cube and Sokooban.


@section{Line search for convex minimization}

@defmodule[lts-cm/delta-secant]

This module implements the Δ-Secant line search algorithm for the paper
@hyperlink["https://arxiv.org/abs/2307.16560"]{``Line Search for Convex Minimization''}.

The function @racket[convex-line-search] returns the lowest point found of a given convex function
between two initial points when a stopping criterion is satisfied.

The function @racket[quasi-exact-line-search] build upon @racket[convex-line-search] to ensure
@emph{sufficient} progress is made, and is intended to be used within an optimization algorithm
such as @hyperlink["https://en.wikipedia.org/wiki/Gradient_descent"]{gradient descent} or
@hyperlink["https://en.wikipedia.org/wiki/Frank%E2%80%93Wolfe_algorithm"]{Frank-Wolfe}.


@defproc[(convex-line-search [f (-> real? real?)] [xleft real?] [xright real?]
                             [#:yleft yleft real? (f xleft)]
                             [#:xq xq real? (* .5 (+ xleft xright))]
                             [#:yq yq real? (f xq)]
                             [#:y-tolerance real? y-tolerance 1.e-10]
                             [#:stop-when stop-when
                              (-> dict? any/c)
                              (λ (dic) (<= (dict-ref dic 'ygap) y-tolerance))]
                             [#:callback callback (-> dict? any/c) (λ (dic) (void))])
         dict?]{
The function @racket[f] is assumed convex between @racket[xleft] and @racket[xright], and the
 behaviour is undefined otherwise.
 Assume that y* = f(x*) = min_x f(x) is the minimum of f on the interval [xleft, xright].

 This function returns a dictionary of values with the following keys:
 @itemize[
 @item{@racket['iter]: Number of iterations performed.}
 @item{@racket['xlow] and @racket['ylow]: lowest point found — usually these are the quantities of
   interest.}
 @item{@racket['xgap] and @racket['ygap]: upper bounds on |xlow - x*| and |ylow - x*|.}
 @item{@racket['x-] and @racket['x+]: x-interval containing x*.}
 @item{@racket['ya] and @racket['yb]: The minimum of these two values is a lower bound on y*.}
 @item{@racket['pts]: The 5 points around x*. See paper.}]

 The arguments @racket[yleft] and @racket[yq] MUST be equal to @racket[(f xleft)] and @racket[(f xq)].

 The argument @racket[xq] is the first point within [@racketid[xleft], @racketid[xright]] to be
 queried.

The argument @racket[stop-when] controls when the algorithm should terminate. By default,
it terminates when the y-distance to the minimum (@racket['ygap]) is provably less than
@racket[y-tolerance].

The argument @racket[callback] can be used to monitor the progress of the line search.

}

@examples[
 #:eval the-eval
 (convex-line-search (λ (x) (sqr (- x 1))) -2 5)
 (define (keep-keys dic keys) (filter (λ (l) (memq (car l) keys)) dic))
 (keep-keys (convex-line-search (λ (x) (sqr (- x 1))) -2 5 #:y-tolerance 0.01)
            '(iter xlow ylow y-gap))
 (keep-keys
  (convex-line-search (λ (x) (max (sqr (- x 1)) (sqr (+ x 1)))) -2 5)
  '(iter xlow ylow xgap ygap))]


@defproc[(quasi-exact-line-search [f (-> real? real?)] [xleft real? 0.] [xright real? 1.]
                                  [#:yleft yleft real? (f xleft)]
                                  [#:xq xq real? (* .5 (+ xleft xright))]
                                  [#:yq yq real? (f xq)]
                                  [#:jac^2 jac^2 (or/c #f positive-real?) #f]
                                  [#:c c positive-real? 1.]
                                  [#:callback callback (-> dict? any/c) (λ (dic) (void))])
         dict?]{
Like @racket[convex-line-search] but the argument @racket[c] controls how close to the minimum
 the returned value @racket[ylow] (within the returned dictionary) should be compared to the initial
 value @racketid[yleft]; more precisely, we have
 @racketid[ylow] - y* ≤ @racketid[c](@racketid[yleft] - @racketid[ylow]).

 Moreover, by contrast to @racket[convex-line-search],
 if the minimum is found to be at @racketid[xright], the range  [@racketid[xleft], @racketid[xright]]
 is quadrupled to the right and the line search continues, and so on.
 This means that for example the call @racket[(quasi-exact-line-search / 1 2)] loops forever.
 To prevent this quadrupling behaviour, one can force the function @racket[f] to be increasing at
 @racket[xright], for eaxmple with  @racket[(λ (x) (if (< x 2) (/ x) +inf.0))]
 instead of @racket[/].


The argument @racket[jac^2], if provided, should be the squared 2-norm of the jacobian (aka the
 gradient or derivative) at @racket[xleft]. This information may be used to speed up the search.

 See @racket[convex-line-search] for the description of the returned dictionary, and of the other
 arguments.
}

@examples[
 #:eval the-eval
 (for/list ([c '(1 10 100)])
   (keep-keys
    (quasi-exact-line-search (λ (x) (sqr (- x 1))) -2 5 #:c c)
    '(iter xlow ylow)))]

@deftogether[
 (@defstruct*[pt ([x real?] [y real?])]
   @defstruct*[(ptg pt) ([g real?])])
 ]{
Points without and with gradient. May be used in the @racket['pts] entry of the return dictionaries
 of @racket[convex-line-search] and @racket[quasi-exact-line-search].
}