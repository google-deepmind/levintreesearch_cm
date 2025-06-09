#lang scribble/manual
@require[@for-label[lts-cm
                    racket/base
                    lts-cm/collector
                    lts-cm/encode
                    lts-cm/byte-board]
         scribble/examples
         lts-cm/delta-secant]

@title{Levin Tree Search with Context Models}
@author{lorseau}

@(define the-eval (make-base-eval '(require racket/math racket/fixnum lts-cm/delta-secant lts-cm/collector lts-cm/encode lts-cm/byte-board)))

@defmodule[lts-cm]

See the @hyperlink["https://github.com/deepmind/levintreesearch_cm#readme"]{README} for introductory
examples for the LTS+CM algorithm on several domains such as Rubik's cube and Sokooban.


@section{Context Models}

@defmodule[lts-cm/collector]

In Levin Tree Search with Context Models (LTS-CM), context models are crucial for guiding the search.
Contexts are pieces of information extracted from the environment or state, which are then used to predict the utility of actions or states. For efficiency, particularly when used as keys in hash tables, these contexts are encoded into fixnums. This encoding process is performed independently for each "mutex set" (a mutually exclusive set of contexts).

While it's technically necessary for each individual encoded context to fit within a Racket fixnum, a more significant practical consideration is the cardinality of each mutex set (i.e., the total number of unique contexts it can produce). If this cardinality is excessively large, learning is likely to be very slow, regardless of whether individual contexts can be encoded. Therefore, designing effective mutex sets with a manageable number of possible contexts is often more critical than merely staying within fixnum limits for a single context's encoding.

Tiling is a common strategy for generating contexts, especially in grid-based environments. The @racketmodname[lts-cm/byte-board] module provides dedicated functions for this. The following example demonstrates how to use @racket[board-relative-tiling/collect] to extract contexts from a small board.

@bold{Example: Relative Tiling with `board-relative-tiling/collect`}

@bold{1. Scenario Setup}

Consider the following 4x5 board state (4 rows, 5 columns):
@verbatim|{
  XXXXX
  X P X  <- Player 'P' at (1,1), Space at (1,2)
  X  GX  <- Space at (2,1), Gold 'G' at (2,2)
  XXXXX
}|
The characters map to byte values: 'X' (wall) = 0, ' ' (empty) = 1, 'P' (player) = 2, 'G' (gold) = 3.
Thus, our @racket[max-val] for encoding will be 3. We use the wall value (0) as @racket[pad-val].

Here's how we create this board and find the player 'P' (byte value 2). The runnable code is shown in the @racket[@examples] block below.
@examples[ #:eval the-eval
(define board-rows
  '("XXXXX"    ; Row 0
    "X P X"    ; Row 1. P is 'P', char before/after is ' '
    "X  GX"    ; Row 2. G is 'G', char before/after is ' '
    "XXXXX"))   ; Row 3
(define n-cols (string-length (first board-rows)))
(define max-val 3)
(define pad-val 0)

(define example-board
  (list->board
   (for*/list ([row (in-list board-rows)]
               [char-in-string (in-string row)])
     (case char-in-string
       [(#\X) 0]
       [(#\space) 1]
       [(#\P) 2]
       [(#\G) 3]
       [else (error "unknown character" char-in-string)]))
   n-cols))

(define-values (p-row p-col) (board-find example-board 2))
(displayln (string-append "Player found at: row " (number->string p-row) ", col " (number->string p-col)))
]

@bold{2. Demonstrating `board-relative-tiling/collect`}
We will use @racket[board-relative-tiling/collect] to extract contexts for a 2x2 tiling scheme centered around the player at (@racket[p-row], @racket[p-col]), with a look-around distance of 1.
Parameters for @racket[board-relative-tiling/collect]:
@itemize[
  @item{@racket[brd]: @racket[example-board].}
  @item{@racket[#:collect! writeln]: We will use @racket[writeln] to print each context fixnum.}
  @item{@racket[#:row p-row], @racket[#:col p-col].}
  @item{@racket[#:max-value max-val].}
  @item{@racket[#:pad-value pad-val].}
  @item{@racket[#:row-dist 1], @racket[#:col-dist 1].}
  @item{@racket[#:row-span 2], @racket[#:col-span 2].}
]
This call will use @racket[#:collect! writeln], causing it to print 4 context fixnums (one for each of the 2x2 tiles whose top-left corners can be (0,0), (0,1), (1,0), and (1,1) relative to the board origin, due to the player position and distances) to the standard output, each on a new line.
The code to perform this is shown in the @racket[@examples] block:
@examples[ #:eval the-eval
(board-relative-tiling/collect example-board
                               #:collect! writeln ;; Using writeln
                               #:row p-row #:col p-col
                               #:max-value max-val #:pad-value pad-val
                               #:row-dist 1 #:col-dist 1
                               #:row-span 2 #:col-span 2)
;; This call prints 4 fixnums to standard output (see explanation below).
(void) ; Suppress #<void> output from the last expression in example block
]

@bold{3. Illustrative Tile Patterns and Their Encodings}
The call to @racket[board-relative-tiling/collect] with @racket[#:collect! writeln] (using player at @racket[(1,1)], @racket[#:row-dist 1], @racket[#:col-dist 1], etc.) processes the 4 tiles whose top-left corners are (0,0), (0,1), (1,0), and (1,1). These are:
@verbatim|{
  Active contexts (tiles) for the player at (1,1) with distance 1, span 2:
  Tile 1 (Top-Left) Tile 2 (Top-Right) Tile 3 (Bottom-Left) Tile 4 (Bottom-Right)
  Top-left: (0,0)   Top-left: (0,1)    Top-left: (1,0)     Top-left: (1,1)

  Visuals & Raw Cell Values (chars from board):
  Board: XXXXX
         X P X  (P at (1,1) is val 2; space at (1,2) is val 1)
         X  GX  (space at (2,1) is val 1; G at (2,2) is val 3)
         XXXXX

  Tile 1  Tile 2  Tile 3  Tile 4
  (0,0)   (0,1)   (1,0)   (1,1)
  ┌──┐    ┌──┐    ┌──┐    ┌──┐
  │XX│    │XX│    │XP│    │P │
  │XP│    │P │    │X │    │ G│
  └──┘    └──┘    └──┘    └──┘
  X,X,X,P X,X,P,' ' X,P,X,' ' P,' ',' ',G

  Corresponding byte values (X=0, ' '=1, P=2, G=3):
  Tile 1: (0 0 0 2)
  Tile 2: (0 0 2 1)
  Tile 3: (0 2 0 1)
  Tile 4: (2 1 1 3)
}|

@bold{4. Cardinality and Encoding of One Tile}

Each of these 2x2 tiles represents a mutex set. Since each of the @racket[(* 2 2)] = 4 cells in a tile can take on one of 4 values (0, 1, 2, or 3), the @italic{cardinality} of each such mutex set is @racket[(expt 4 (* 2 2))] = 4^4 = 256.
This means each context fixnum generated for these tiles will be a number between 0 and 255 (inclusive).

Let's explicitly encode the pattern for @bold{Tile 1} (values `(0 0 0 2)`). The following runnable example shows this, and also calculates the encoding for Tile 4 for comparison:
@examples[ #:eval the-eval
(define max-val-plus-1 (add1 max-val))
(define cell-sizes (list max-val-plus-1 max-val-plus-1 max-val-plus-1 max-val-plus-1))

(define tile1-values '(0 0 0 2))
(define tile1-fixnum (naturals->fixnum tile1-values cell-sizes))
(displayln (string-append "Tile 1 (0002) manually encodes to: " (number->string tile1-fixnum)))

(define tile4-values '(2 1 1 3))
(define tile4-fixnum (naturals->fixnum tile4-values cell-sizes))
(displayln (string-append "Tile 4 (2113) manually encodes to: " (number->string tile4-fixnum)))
(displayln "The call to board-relative-tiling/collect with writeln would print these (and others) to output.")
(void)
]

The call to @racket[board-relative-tiling/collect] with @racket[#:collect! writeln] will print four fixnums to standard output. These correspond to Tile 1, Tile 2, Tile 3, and Tile 4, in that order (due to row-major iteration of tile positions). The following example calculates the expected fixnum values for all four tiles:
@examples[ #:eval the-eval
(define max-val-plus-1 (add1 max-val))
(define cell-sizes (list max-val-plus-1 max-val-plus_1 max-val-plus_1 max-val-plus_1))

(define tile1-calc-values '(0 0 0 2))
(define tile2-calc-values '(0 0 2 1))
(define tile3-calc-values '(0 2 0 1))
(define tile4-calc-values '(2 1 1 3))

(define all-4-calculated-fixnums
  (list (naturals->fixnum tile1-calc-values cell-sizes)
        (naturals->fixnum tile2-calc-values cell-sizes)
        (naturals->fixnum tile3-calc-values cell-sizes)
        (naturals->fixnum tile4-calc-values cell-sizes)))
(displayln (string-append "The call using #:collect! writeln would print each of these on a new line (in order):"))
(displayln (format "~a" all-4-calculated-fixnums))
all-4-calculated-fixnums
]
This example illustrates how specific tile patterns around a point of interest can be converted into unique fixnum context identifiers.

@subsection{Collector Utilities for Custom Logic}

The @racketmodname[lts-cm/collector] module offers general-purpose utilities for gathering context fixnums. These are primarily useful when you are implementing custom logic for a domain's @racket[collect-contexts] function, or in other scenarios where contexts need to be programmatically collected into specific data structures (lists or fxvectors).

For some common context generation functions, like @racket[board-relative-tiling/collect] shown in the preceding example, you can often use a direct procedure like @racket[writeln] with the @racket[#:collect!] argument if your goal is simply to print the contexts. Similarly, a small custom lambda function can be sufficient for immediate processing. However, if you need to accumulate contexts from multiple sources or manage them in a more complex way before further processing, these collector utilities provide helpful abstractions.

The `lts-cm/collector` module provides helper functions to facilitate the collection of these encoded contexts during the search process. These collectors are typically passed to a domain-specific `collect-contexts` function.

@defproc[(make-list-collector) (-> (or/c null? procedure?))]
{
  Creates a collector that gathers context fixnums into a list.
  A domain's `collect-contexts` function, responsible for extracting features from a state, would be provided with an instance of this collector (typically as a procedure). For each relevant feature identified and encoded into a fixnum, `collect-contexts` would call this procedure with the feature fixnum, e.g., @racket[(my-collector feature-1-fixnum)], @racket[(my-collector feature-2-fixnum)], etc.
  After `collect-contexts` has called it for all features, the system can then retrieve the complete list of contexts by calling the collector instance with no arguments.
  This collector is straightforward but may be less efficient for very large numbers of contexts due to list operations.

  The following demonstrates the collector's behavior, which `collect-contexts` would rely on:
  @examples[
    #:eval the-eval
    (define lc (make-list-collector))
    ; Inside collect-contexts, one might see:
    (lc 101) ; Call for first feature
    (lc 102) ; Call for second feature
    (lc 103) ; Call for third feature
    ; Later, the system calls:
    (lc)     ; Returns '(103 102 101)
  ]
}

@defproc[(make-fxvector-collector [vec fxvector?]) (-> (-> any/c void?))]
{
  Creates a collector designed to populate a pre-allocated @racket[fxvector] with context fixnums.
  This is useful when the number of contexts to be collected is known in advance, allowing for efficient storage.
  A domain's `collect-contexts` function would receive the procedure returned by @racket[make-fxvector-collector] (let's call this procedure @racket[store-feature!]). For each feature fixnum, `collect-contexts` would invoke @racket[(store-feature! feature-fixnum)].
  The `collect-contexts` implementation must ensure it does not call @racket[store-feature!] more times than the length of the provided @racket[vec]. The vector is filled sequentially. After `collect-contexts` finishes, the original @racket[vec] will contain the collected contexts.
  This collector is highly efficient as it directly mutates the existing vector.

  The behavior of the returned procedure is shown below:
  @examples[
    #:eval the-eval
    (define VEC_SIZE 3)
    (define context-vector (make-fxvector VEC_SIZE 0))
    (define store-in-vector (make-fxvector-collector context-vector))
    ; Inside collect-contexts, for a domain that always produces VEC_SIZE features:
    (store-in-vector 201)
    (store-in-vector 202)
    (store-in-vector 203)
    ; After collect-contexts returns, context-vector is now #(201 202 203)
    context-vector
  ]
}

@defproc[(make-fxvector-collector/auto) (-> (or/c fxvector? procedure?))]
{
  Creates a flexible collector that gathers context fixnums internally and then produces an @racket[fxvector] upon request.
  Similar to @racket[make-list-collector], a domain's `collect-contexts` function would receive an instance of this collector. It would call this instance for each feature fixnum, e.g., @racket[(my-collector feature-1-fixnum)].
  Once `collect-contexts` has processed all features, the system retrieves an @racket[fxvector] of these contexts by calling the collector instance with no arguments.
  This collector is convenient when the number of contexts is not known beforehand, but it is slower than @racket[make-fxvector-collector] due to intermediate list allocations and the final conversion to an @racket[fxvector]. It can be particularly useful during development and debugging.

  The following demonstrates its two-phase operation (collection and retrieval):
  @examples[
    #:eval the-eval
    (define fxa (make-fxvector-collector/auto))
    ; Inside collect-contexts:
    (fxa 301)
    (fxa 302)
    (fxa 303)
    ; Later, the system calls:
    (fxa) ; returns #(301 302 303)
  ]
}

@section{Fixnum Encoding}

@defmodule[lts-cm/encode]

The `lts-cm/encode` module provides utilities for encoding lists of natural numbers (typically representing context features or parts of a state) into a single fixnum, and vice-versa. This is essential for creating compact representations that can be used as keys in hash tables or for other efficient processing. The encoding scheme is akin to representing a number in a mixed radix system, where each position can have a different base (size).

@defproc[(naturals->fixnum [ints (listof natural?)] [sizes (listof exact-positive-integer?)] [n fixnum? 0]) fixnum?]
{
  Encodes a list of natural numbers, @racket[ints], into a single fixnum. The @racket[sizes] list specifies the maximum value (plus one) for the corresponding integer in @racket[ints]. The encoding is performed sequentially, and an optional initial fixnum @racket[n] can be provided to chain encodings.

  Each integer @racket[i] from @racket[ints] must be less than its corresponding @racket[s] in @racket[sizes] (i.e., @racket[0 <= i < s]). The function folds from left to right, effectively computing @racket[(((n * size_0 + int_0) * size_1 + int_1) * ...)].

  @examples[
    #:eval the-eval
    (naturals->fixnum '(0 2 1 2 2 0 1 0) '(2 3 4 5 6 7 8 9))
    ; Hierarchical encoding:
    (define base-code (naturals->fixnum '(0 2 1 2 2) '(2 3 4 5 6)))
    (naturals->fixnum '(0 1 0) '(7 8 9) base-code)
  ]
}

@subsection{Rubik's Cube Inspired Example}

Let's consider a simplified scenario for encoding parts of a Rubik's Cube state. Suppose we want to encode:
@itemize[
  @item{The permutation of 3 specific edge cubies (e.g., UB, UR, UF). We need to know their current locations out of 12 possible edge locations.
    Let's say Edge1 (e.g., UB) can be in any of 12 locations.
    Edge2 (e.g., UR) can then be in any of the remaining 11 locations.
    Edge3 (e.g., UF) can then be in any of the remaining 10 locations.}
  @item{The orientation of these 3 edge cubies. Each edge cubie can have 2 orientations (flipped or not flipped).}
]

We can define our features and their corresponding sizes (maximum values + 1):
@itemize[
  @item{`loc1`: Location of Edge1 (value 0-11, size 12)}
  @item{`loc2`: Location of Edge2 from remaining (value 0-10, size 11)}
  @item{`loc3`: Location of Edge3 from remaining (value 0-9, size 10)}
  @item{`ori1`: Orientation of Edge1 (value 0-1, size 2)}
  @item{`ori2`: Orientation of Edge2 (value 0-1, size 2)}
  @item{`ori3`: Orientation of Edge3 (value 0-1, size 2)}
]

Suppose we have the following state for these features:
- Edge1 is in slot 5.
- Edge2 is in slot 2 (of the remaining 11 slots).
- Edge3 is in slot 7 (of the remaining 10 slots).
- Edge1 is not flipped (orientation 0).
- Edge2 is flipped (orientation 1).
- Edge3 is not flipped (orientation 0).

The list of values would be @racket['(5 2 7 0 1 0)], and the list of sizes would be @racket['(12 11 10 2 2 2)].
We can encode this into a single fixnum:

@examples[
  #:eval the-eval
  (define edge-features '(5 2 7 0 1 0))
  (define feature-sizes '(12 11 10 2 2 2))
  (naturals->fixnum edge-features feature-sizes)
]

This single fixnum now represents this combined state of permutation and orientation for the three chosen edges.
Note that this is a highly simplified example. A complete Rubik's Cube state representation would involve encoding all cubie permutations and orientations, often using more sophisticated algorithms and larger numbers, but the principle of combining features into a single number using their respective sizes remains similar. The `lts-cm/encode` module provides the basic tools for such custom encodings.

@defproc[(fixnum->naturals [n-orig fixnum?] [sizes (listof exact-positive-integer?)]
                           [#:? remainder (or/c #t #f 'check-0 'cons) 'check-0])
         (cond
           [(eq? remainder #t) (values (listof natural?) fixnum?)]
           [(eq? remainder 'cons) (listof natural?)]
           [else (listof natural?)])]
{
  Decodes a fixnum, @racket[n-orig], back into a list of natural numbers, given the list of @racket[sizes] used for encoding. The @racket[sizes] list is processed in reverse order for decoding, corresponding to how @racket[naturals->fixnum] performs the encoding.

  The @racket[remainder] argument controls how any remaining value of @racket[n-orig] after decoding with the given @racket[sizes] is handled:
  @itemize[
    @item{@racket['check-0] (default): Raises an error if the remainder is not zero, ensuring the fixnum is fully decoded by the given sizes.}
    @item{@racket[#f]: The remainder is discarded.}
    @item{@racket[#t]: Returns two values: the list of decoded naturals and the remainder.}
    @item{@racket['cons]: The remainder is @racket[cons]'ed onto the beginning of the resulting list of naturals.}
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

@defform[(naturals->fixnum* [n fixnum? 0] [val natint?] [size posint?] ...+)]{
  A convenience syntax (macro) for encoding sequences of @racket[[value size]] pairs.
  It is equivalent to chained calls to an internal @racket[natural-encode1] helper, which performs @racket[(fx+ (fx* current-encoding size) value)].
  An optional initial fixnum @racket[n] can be provided.

  @examples[
    #:eval the-eval
    (naturals->fixnum* [0 2] [2 3] [1 4] [2 5] [2 6] [0 7] [1 8] [0 9])
    ; Starting with a base value:
    (naturals->fixnum* 5589 [0 2] [2 3] [1 4])
  ]
}

@defproc[(bytes-context/encode [bts bytes?] [idxs fxvector?] #:! [max-value byte?]) fixnum?]
{
  Encodes byte values extracted from a @racket[bytes] object, @racket[bts], at indices specified by the @racket[fxvector] @racket[idxs].
  The @racket[max-value] parameter specifies the maximum possible value for a byte being encoded (e.g., 255). The encoding base for each byte will be @racket[(add1 max-value)].
  This function is useful for creating a single fixnum context from multiple byte-sized features in a state representation.
  The values actually read from @racket[bts] at @racket[idxs] must not exceed @racket[max-value].

  @examples[
    #:eval the-eval
    (define data (bytes 10 20 30 40 50))
    (define indices (fxvector 0 2 4))
    (bytes-context/encode data indices #:max-value 255) ; encodes (10, 30, 50)
    ; Example with smaller max-value
    (define data2 (bytes 1 0 2))
    (bytes-context/encode data2 (fxvector 0 1 2) #:max-value 2)
    ; (bytes-context/encode data2 (fxvector 0 1 2) #:max-value 1) ; This would be problematic as data2 contains 2
  ]
}

@section{Byte Board Utilities}

@defmodule[lts-cm/byte-board]

This module provides utilities for working with 2D boards represented by byte strings, primarily for generating contexts using tiling schemes. A board is a structure holding a flat byte string along with its dimensions.

@deftogether[(
  @defstruct[board ([vec bytes?] [n-rows exact-positive-integer?] [n-cols exact-positive-integer?])]
)]{
  Represents a 2D board. The @racket[vec] field stores the board's cell values in a flat byte string (row-major order). Cell values are thus restricted to 0-255.
  Many utility functions like @racket[make-board], @racket[board-ref], @racket[board-set!], @racket[list->board] are provided by the @racketmodname[lts-cm/byte-board] module.
}


@defproc[(board-relative-tiling/collect [brd board?]
                                        [#:collect! collect! (-> fixnum? any/c)]
                                        [#:row row0 exact-integer?]
                                        [#:col col0 exact-integer?]
                                        [#:? max-value byte? 255]
                                        [#:? pad-value byte? max-value]
                                        [#:? row-dist exact-positive-integer? 1]
                                        [#:? col-dist exact-positive-integer? row-dist]
                                        [#:? row-span exact-positive-integer? 2]
                                        [#:? col-span exact-positive-integer? row-span])
         void?]
{
  Collects context fixnums generated by applying a relative tiling scheme on the given @racket[brd] around a central point (@racket[row0], @racket[col0]). This function does not return a useful value; its primary effect is achieved through repeated calls to the @racket[collect!] procedure.

  @bold{Behavior Details:}
  The function iterates through a grid of tile positions. This grid is defined relative to the central point (@racket[row0], @racket[col0]) and controlled by the distance and span parameters:
  @itemize[
    @item{The top-left corner of a tile can be shifted from @racket[(- row0 row-dist)] up to @racket[(- (+ row0 (- row-dist row-span)) -1)] vertically.}
    @item{Similarly, it can be shifted from @racket[(- col0 col-dist)] up to @racket[(- (+ col0 (- col-dist col-span)) -1)] horizontally.}
    @item{The total number of such tile positions (and thus, the number of mutex sets processed by this call for this specific tiling configuration) is @racket[(* (+ row-dist row-dist (- row-span) 2) (+ col-dist col-dist (- col-span) 2))] if using the source code's range, or more intuitively, @racket[(* (row-dist + 1 + row-dist - row-span +1) (col-dist + 1 + col-dist - col-span +1))] if considering number of discrete steps. The exact number of tiles is determined by the iteration ranges shown in the source of @racketmodname[lts-cm/byte-board].}
  ]

  For each of these tile positions:
  @itemlist[
    @item{A pattern of cell values is read from @racket[brd]. The dimensions of this pattern are determined by @racket[row-span] and @racket[col-span] (e.g., a @racket[row-span] x @racket[col-span] tile).}
    @item{If any cell of the current tile falls outside the bounds of @racket[brd], its value is taken as @racket[pad-value].}
    @item{This pattern of (potentially padded) cell values is then encoded into a single fixnum. The encoding assumes that each cell value is a natural number less than or equal to @racket[max-value]. The base for encoding each cell is therefore @racket[(add1 max-value)]. This process is similar to using @racket[naturals->fixnum] (see @secref["Fixnum-Encoding"]).}
    @item{The provided @racket[collect!] procedure is then called with this single generated fixnum.}
  ]

  Each call to @racket[collect!] signifies an "active context" from one of the mutex sets defined by this particular relative tiling configuration. The @racket[collect!] procedure is responsible for handling this context fixnum (e.g., storing it, incrementing its count, etc.).

  For example, if @racket[row-dist] and @racket[col-dist] are both 1, and @racket[row-span] and @racket[col-span] are both 2 (a 2x2 tile), this function will consider 9 possible 2x2 tiles arranged in a 3x3 grid centered around (@racket[row0], @racket[col0]). It will then call @racket[collect!] 9 times, each time with a fixnum representing the pattern of one of these 2x2 tiles.
}

@defproc[(list->board [lst (listof byte?)] [n-cols exact-positive-integer?])
         board?]
{
  Creates a board from a flat list of byte values, @racket[lst]. The board will have @racket[n-cols] columns.
  If the length of @racket[lst] is not a multiple of @racket[n-cols], the list is effectively truncated to the largest multiple of @racket[n-cols] that fits, and the remaining elements are ignored.
  This function is used in examples to construct board instances.
}

@defproc[(board-find [aboard board?] [x byte?])
         (values (or/c false/c exact-integer?) (or/c false/c exact-integer?))]
{
  Finds the first occurrence of the byte value @racket[x] in @racket[aboard], searching in row-major order (left-to-right, then top-to-bottom).
  Returns two values: the row and column of the first occurrence of @racket[x]. If @racket[x] is not found in the board, it returns @racket[(values #f #f)].
}

@defproc[(board->string [aboard board?])
         string?]
{
  Converts the @racket[aboard] to a multi-line string representation, suitable for printing to the console.
  Internally, this often uses a table-formatting utility to align cell values.
}

@defproc[(board->list [aboard board?])
         (listof byte?)]
{
  Converts the board's internal byte vector into a new flat list of byte values, representing the board's cells in row-major order.
}

@defproc[(board-in-bounds? [brd board?] [row exact-integer?] [col exact-integer?])
         boolean?]
{
  Checks if the given @racket[row] and @racket[col] coordinates are within the valid bounds of the board @racket[brd].
  Returns @racket[#t] if @racket[(0 <= row < (board-n-rows brd))] and @racket[(0 <= col < (board-n-cols brd))], and @racket[#f] otherwise.
}

@defproc[(board-set! [aboard board?] [row exact-integer?] [col exact-integer?] [val byte?])
         void?]
{
  Sets the value of the cell at (@racket[row], @racket[col]) in @racket[aboard] to @racket[val].
  This function mutates the input @racket[aboard].
  It will typically raise an error if the coordinates are out of bounds or if @racket[val] is not a byte.
}

@defform[(board-index aboard row col)]
{
  A macro (syntax-rule) for calculating the 1D index into the board's internal flat byte vector that corresponds to the 2D coordinates (@racket[row], @racket[col]).
  @racket[aboard] must be an instance of @racket[board?], and @racket[row] and @racket[col] must be exact integers.
  This is primarily an internal utility but can be useful for optimized board manipulations.
}

@defproc[(board-copy [brd board?])
         board?]
{
  Creates and returns a new board that is a (deep) copy of the input @racket[brd].
  This means the internal byte vector storing the cell data is also copied, so modifications to the new board will not affect the original, and vice-versa.
}

@defproc[(board->bytes [aboard board?])
         bytes?]
{
  Returns the internal byte vector (a @racket[bytes?] object) that stores the cell data for @racket[aboard].
  @bold{Important}: This function returns the actual internal byte string, not a copy. Therefore, modifications to the returned byte string will directly affect the @racket[aboard] from which it was obtained. For a safe copy, use @racket[(bytes-copy (board->bytes aboard))] or create a new board via @racket[board-copy].
}

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