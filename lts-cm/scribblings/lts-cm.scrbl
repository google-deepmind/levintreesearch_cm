#lang scribble/manual
@(require (for-label lts-cm
                     racket/base
                     racket/string
                     racket/list
                     racket/fixnum
                     lts-cm/collector
                     lts-cm/encode
                     lts-cm/byte-board)
          racket/runtime-path
          scribble/examples
          scriblib/footnote
          lts-cm/delta-secant)

@title{Levin Tree Search with Context Models}
@author{Laurent Orseau}

@(define-runtime-path img-dir "img")

@(define the-eval
   (make-base-eval
    '(require racket/math
              racket/list
              racket/fixnum
              lts-cm/delta-secant
              lts-cm/collector
              lts-cm/encode
              lts-cm/byte-board)))

@defmodule[lts-cm]

See the @hyperlink["https://github.com/deepmind/levintreesearch_cm#readme"]{README} for introductory
examples for the LTS+CM algorithm on several domains such as Rubik's cube and Sokooban.


@section{Context Models}

In Levin Tree Search with Context Models (LTS+CM), context models are crucial for guiding the search.
Contexts are pieces of information extracted from the environment or state, which are then used to
predict which actions to take (the policy).
Contexts are grouped into mutually exclusive sets called @emph{mutex sets}.
The number of mutex sets is assumed fixed during the search, but the number of contexts per mutex sets
does not need to be known.

For efficiency though, each context is encoded into a Racket fixnum --- let's call this the context's
fixnum.
The learnable parameters β of the context models are stored in a Context DataBase (CDB), as a matrix
where each row is a context fixnum and each column is an action.
Each mutex set is associated with a hash table where the key is the context fixnum, and the value
is the row in the βmatrix.


@bold{Isn't encoding into a fixnum restrictive?}
Not really. Having too many contexts per mutex sets can be detrimental to learning.
For example, suppose that we have N datapoints and that each mutex sets has C contexts.
Due to the mutual exclusion nature of mutex sets, each datapoint is associated with exactly one
context per mutex set. Hence, each context may receive about N/C datapoints.
As a crude rule of thumb, each context may need a few dozens of datapoints for their associated β
parameters to learn good values.
@note{In practice, context occurrences within a mutex set are more likely to follow a Zipf
distribution, which changes the argument a little, but the main idea still stands.}
Thus, if C is large, N must be large too.


@bold{Example: Relative Tiling with @racket[board-relative-tiling/collect]}

Tiling is a common strategy for generating contexts, especially in grid-based environments.
The @racketmodname[lts-cm/byte-board] module provides dedicated functions for this.
The following example demonstrates how to use @racket[board-relative-tiling/collect]
to extract contexts from a small board.

Consider a very simple game where the player 'P' must get the gold 'G' on a 2D grid.
The grid has 4 rows and 5 columns and is surrounded by walls ('X').
Each cell can thus take 4 different values (including empty ' ').
@examples[ #:eval the-eval
(define board-rows
  '("XXXXX"
    "X P X"
    "X  GX"
    "XXXXX"))
(code:line (define n-cols (string-length (first board-rows))) @code:comment["= 4"])

@code:comment["Let's convert the string into a board of integers:"]
(define board-as-list
  (for*/list ([row (in-list board-rows)] [chr (in-string row)])
     (case chr
       [(#\X)     0]
       [(#\space) 1]
       [(#\P)     2]
       [(#\G)     3]
       [else (error "unknown character" chr)])))

(define a-board (list->board board-as-list n-cols))
]

Let's use @racket[board-relative-tiling/collect] to extract contexts for a 2x2 tiling scheme centered
around the player at (@racket[p-row], @racket[p-col]), with a look-around distance of 1.
This call uses @racket[#:collect! writeln], causing it to print the 4 context fixnums --- one per
mutex set, that is, one per tile of size 2x2:
@examples[
 #:label #f
 #:eval the-eval
(code:line (define max-val 3) @code:comment["0: wall, 1: empty, 2: player, 3: gold"])
(code:line (define pad-val 0) @code:comment["= wall"])
@code:comment{Let's find the player P=2:}
(code:line (define-values (p-row p-col) (board-find a-board 2)) @code:comment[" = (1, 2)"])
(board-relative-tiling/collect a-board
                               #:collect! displayln
                               #:row p-row #:col p-col
                               #:max-value max-val #:pad-value pad-val
                               #:row-dist 1 #:col-dist 1
                               #:row-span 2 #:col-span 2)]
This prints the 4 fixnums of the active contexts of each tile (mutex set):
@verbatim|{
  Tile 1  Tile 2  Tile 3  Tile 4
  ┌──┐    ┌──┐    ┌──┐    ┌──┐
  │XX│    │XX│    │ P│    │P │
  │ P│    │P │    │  │    │ G│
  └──┘    └──┘    └──┘    └──┘
}|
Let's see how to calculate these fixnums by hand.
If size = max-val + 1, the fixnum of the context of the first tile is
@verbatim|{
fixnum of tile 1 = (((wall × size) + wall) × size + empty) × size + player
                 = (((0 × 4) + 0) × 4 + 1) × 4 + 2
                 = 6
}|
The other context fixnums are calculated similarly.
The same calculation can be done more simply using @racket[naturals->fixnum*]:
@examples[
 #:label #f
 #:eval the-eval
 (define wall 0)
 (define empty-cell 1)
 (define player 2)
 (define gold 3)
 (define size 4)
 @code:comment["Tile 1:"]
 (naturals->fixnum* [wall size] [wall size] [empty-cell size] [player size])
 @code:comment["Tile 2:"] 
 (naturals->fixnum* [wall size] [wall size] [player size] [empty-cell size])]

Note that encodings are local to each mutex set, that is, even if two active contexts of different
tiles have the same fixnums, they will map to different rows in the βmatrix.

@section{Collectors}
@defmodule[lts-cm/collector]

Instead of @racket[#:collect! displayln], LTS+CM uses a collector that is typically passed as argument
to the domain's custom @racket[collect-context], for example:
@examples[
 #:label #f
 #:eval the-eval
(define collect! (make-list-collector))
(board-relative-tiling/collect a-board
                               #:collect! collect!
                               #:row p-row #:col p-col
                               #:max-value max-val #:pad-value pad-val
                               #:row-dist 1 #:col-dist 1
                               #:row-span 2 #:col-span 2)
 (code:comment "Obtain the collected fixnums:")
 (collect!)]

@defproc[(make-list-collector) (-> procedure?)]{
  Creates a collector that gathers items into a list.
@examples[
 #:eval (make-base-eval '(require lts-cm/collector))
 (define collect! (make-list-collector))
 (collect! 'a)
 (collect! 1)
 (collect! '(x y z))
 (code:comment "Obtain the collected elements:")
 (collect!)]
}

@defproc[(make-fxvector-collector [vec fxvector?]) procedure?]{
  Creates a collector designed to populate a pre-allocated @racket[fxvector] to collect fixnums.
 @examples[
 #:eval (make-base-eval '(require lts-cm/collector racket/fixnum))
 (define vec (make-fxvector 3))
 (define collect! (make-fxvector-collector vec))
 (collect! 1)
 (collect! 2)
 (collect! 3)
 (code:comment "Obtain the collected elements:")
 vec]
}

@defproc[(make-fxvector-collector/auto) procedure?]{
 Similar to @racket[make-fxvector-collector] but for a fxvector of initially unknown size.
 @examples[
 #:eval (make-base-eval '(require lts-cm/collector racket/fixnum))
 (define collect! (make-fxvector-collector/auto))
 (collect! 1)
 (collect! 2)
 (collect! 3)
 (code:comment "Obtain the collected elements:")
 (collect!)]
}

@section{Fixnum Encoding}

@defmodule[lts-cm/encode]

This module provides utilities for encoding lists of natural numbers into a single fixnum, and
vice-versa.
This is essential for creating compact representations that can be used as keys in hash tables or for
other efficient processing.
Technically, the encoding scheme is akin to representing a number in a mixed radix system, where each
position can have a different base (size).

@defproc[(naturals->fixnum [ints (listof natural?)]
                           [sizes (listof exact-positive-integer?)]
                           [n fixnum? 0])
         fixnum?]{
  Encodes a list of natural numbers, @racket[ints], into a single fixnum.
 The @racket[sizes] list specifies the maximum value (plus one) for the corresponding integer in
 @racket[ints]. The encoding is performed sequentially, and an optional initial fixnum @racket[n]
 can be provided to chain encodings.

  Each integer @racket[i] from @racket[ints] must be less than its corresponding @racket[s] in
 @racket[sizes] (i.e., @racket[0 <= i < s]). The function folds from left to right, effectively
 computing @emph{(((n * size_0 + int_0) * size_1 + int_1) * ...)}.

  @examples[
    #:eval the-eval
    (naturals->fixnum '(0 2 1 2 2 0 1 0) '(2 3 4 5 6 7 8 9))
    ; Hierarchical encoding:
    (define base-code (naturals->fixnum '(0 2 1 2 2) '(2 3 4 5 6)))
    (naturals->fixnum '(0 1 0) '(7 8 9) base-code)
  ]
}

@defproc[(fixnum->naturals [n-orig fixnum?] [sizes (listof exact-positive-integer?)]
                           [#:remainder remainder (or/c #t #f 'check-0 'cons) 'check-0])
         (if (eq? remainder #true)
           (values (listof natural?) fixnum?)
           (listof natural?))]{
  Decodes a fixnum, @racket[n-orig], back into a list of natural numbers,
 given the list of @racket[sizes] used for encoding. The @racket[sizes] list is processed in reverse
 order for decoding, corresponding to how @racket[naturals->fixnum] performs the encoding.

  The @racket[remainder] argument controls how any remaining value of @racket[n-orig] after decoding
 with the given @racket[sizes] is handled:
  @itemize[
    @item{@racket['check-0] (default): Raises an error if the remainder is not zero, ensuring the
   fixnum is fully decoded by the given sizes.}
    @item{@racket[#f]: The remainder is discarded.}
    @item{@racket[#t]: Returns two values: the list of decoded naturals and the remainder.}
    @item{@racket['cons]: The remainder is @racket[cons]'ed onto the beginning of the resulting list
   of naturals.}
  ]

  @examples[
    #:eval the-eval
    (fixnum->naturals 143145 '(2 3 4 5 6 7 8 9))
    ; Decoding with remainder:
    (fixnum->naturals 134145 '(2 3 4) #:remainder #t)
    (fixnum->naturals 38 '(7) #:remainder #f)
    (fixnum->naturals 38 '(7) #:remainder 'cons)
    ; Example showing binary decomposition (least significant bits first in list)
    (fixnum->naturals 23 '(2 2 2 2 2)) ; 23 = 10111_2
  ]
}

@defform[(naturals->fixnum* [n fixnum? 0] [[val natint?] [size posint?]] ...+)]{
  A convenience syntax (macro) for encoding sequences of @racket[[value size]] pairs,
  equivalent to @racket[(naturals->fixnum (val ...) (size ...) n)].

  @examples[
    #:eval the-eval
    (naturals->fixnum* [0 2] [2 3] [7 12] [6 10])
     @code:comment["Starting with a base value:"]
     (define n0 (naturals->fixnum* [0 2] [2 3]))
    (naturals->fixnum* n0 [7 12] [6 10])
  ]
}

@;{defproc[(bytes-context/encode [bts bytes?] [idxs fxvector?] [#:max-value max-value byte?])
 fixnum?]{
  Encodes byte values extracted from @racket[bts] at indices specified by the @racket[idxs].
  The @racket[max-value] parameter specifies the maximum possible value for a byte being encoded
 (e.g., 255). The encoding base for each byte will be @racket[(add1 max-value)].
  This function is useful for creating a single fixnum context from multiple byte-sized features in
 a state representation.
  The values actually read from @racket[bts] at @racket[idxs] must not exceed @racket[max-value].

  @examples[
    #:eval the-eval
    (define data (bytes 10 20 30 40 50))
    (define indices (fxvector 0 2 4))
    (bytes-context/encode data indices #:max-value 255) ; encodes (10, 30, 50)
    ; Example with smaller max-value
    (define data2 (bytes 1 0 2))
    (bytes-context/encode data2 (fxvector 0 1 2) #:max-value 2)
    ; (bytes-context/encode data2 (fxvector 0 1 2) #:max-value 1) ; This would be problematic as
 data2 contains 2
  ]
}}

@section{Byte Board Utilities}

@defmodule[lts-cm/byte-board]

This module provides utilities for working with 2D boards represented by byte strings
A board is a structure holding a flat byte string along with its dimensions.

@deftogether[(
  @defstruct*[board ([vec bytes?] [n-rows exact-positive-integer?] [n-cols exact-positive-integer?])]
)]{
  Represents a 2D board. The @racket[vec] field stores the board's cell values in a flat byte string
 (row-major order). Cell values are thus restricted to 0-255.
}


@defproc[(list->board [lst (listof byte?)] [n-cols exact-positive-integer?])
         board?]{
  Creates a board from a flat list of byte values, @racket[lst]. The board will have @racket[n-cols]
 columns.
  If the length of @racket[lst] is not a multiple of @racket[n-cols], the list is effectively
 truncated to the largest multiple of @racket[n-cols] that fits, and the remaining elements are
 ignored.
}

@defproc[(board-find [aboard board?] [x byte?])
         (values (or/c false/c exact-integer?) (or/c false/c exact-integer?))]{
  Finds the first occurrence of the byte value @racket[x] in @racket[aboard], searching in row-major
 order (left-to-right, then top-to-bottom).
  Returns two values: the row and column of the first occurrence of @racket[x]. If @racket[x] is not
 found in the board, it returns @racket[(values #f #f)].
}

@defproc[(board->string [aboard board?])
         string?]{
  Converts the @racket[aboard] to a multi-line string representation, suitable for printing to the
 console.
  @examples[
 #:eval (make-base-eval '(require lts-cm/byte-board racket/list))
 (define brd (list->board (range 9) 3))
 (displayln (board->string brd))
 ]
}

@defproc[(board->list [aboard board?])
         (listof byte?)]{
 Converse of @racket[list->board].
}

@defproc[(board-in-bounds? [brd board?] [row exact-integer?] [col exact-integer?])
         boolean?]{
  Checks if the given @racket[row] and @racket[col] coordinates are within the valid bounds of the
 board @racket[brd].
  Returns @racket[#t] if @racket[(0 <= row < (board-n-rows brd))] and
 @racket[(0 <= col < (board-n-cols brd))], and @racket[#f] otherwise.
}

@defproc[(board-set! [aboard board?] [row exact-integer?] [col exact-integer?] [val byte?])
         void?]{
  Sets the value of the cell at (@racket[row], @racket[col]) in @racket[aboard] to @racket[val].
}

@defform[(board-index aboard row col)]{
  A macro for calculating the 1D index into the board's internal flat byte vector that
 corresponds to the 2D coordinates (@racket[row], @racket[col]).
  @racket[aboard] must be an instance of @racket[board?], and @racket[row] and @racket[col] must be
 exact integers.
  This is primarily an internal utility but can be useful for optimized board manipulations.
}

@defproc[(board-copy [brd board?])
         board?]{
  Creates and returns a new board that is a (deep) copy of the input @racket[brd].
}

@defproc[(board->bytes [aboard board?])
         bytes?]{
  Returns the internal byte vector (a @racket[bytes?] object) that stores the cell data for
 @racket[aboard].
  @bold{Important}: This function returns the actual internal byte string, not a copy. Therefore,
 modifications to the returned byte string will directly affect the @racket[aboard] from which it was
 obtained. For a safe copy, use @racket[(bytes-copy (board->bytes aboard))] or create a new board via
 @racket[board-copy].
}


@defproc[(board-relative-tiling/collect [brd board?]
                                        [#:collect! collect! (-> fixnum? any/c)]
                                        [#:row row0 exact-integer?]
                                        [#:col col0 exact-integer?]
                                        [#:max-value max-value byte? 255]
                                        [#:pad-value pad-value byte? max-value]
                                        [#:row-dist row-dist exact-positive-integer? 1]
                                        [#:col-dist col-dist exact-positive-integer? row-dist]
                                        [#:row-span row-span exact-positive-integer? 2]
                                        [#:col-span col-span exact-positive-integer? row-span])
         void?]{
  Collects context fixnums generated by applying a relative tiling scheme on the given @racket[brd]
 around a central point (@racket[row0], @racket[col0]).
 The context fixnums are collected through repeated calls to the @racket[collect!] procedure.
 The arguments are best described with a picture:
 
 @(image (build-path img-dir "tiling.svg") #:scale 2.5)

 The number of tiles (mutex sets) generated by such a tiling is
 @emph{(row-dist × 2 + 1 - row-span)
  ×
  (col-dist × 2 + 1 - col-span)}.

 For each tile, the cells of the tile are encoded into a single fixnum
 using @racket[naturals->fixnum] with @racket[size] = @racket[(+ max-value 1)].
 If a cell of a tile is outside the boundaries of the board, the @racket[pad-value] is used
 in place of the cell's value.
 The resulting code is passed to @racket[collect!].
}

@section{Line search for convex minimization}

@defmodule[lts-cm/delta-secant]

This module implements the Δ-Secant line search algorithm from the paper
@hyperlink["https://arxiv.org/abs/2307.16560"]{``Line Search for Convex Minimization''}.

The function @racket[convex-line-search] returns the lowest point found of a given convex function
between two initial points when a stopping criterion is satisfied.

The function @racket[quasi-exact-line-search] builds upon @racket[convex-line-search] to ensure
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