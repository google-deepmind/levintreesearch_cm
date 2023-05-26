#lang scribble/manual
@require[@for-label[timev
                    racket/base
                    global]
         scribble/example]

@title{timev}
@author{Laurent Orseau}

@defmodule[timev]

@racket[timev] is a more flexible variant of @racket[time].

@defproc[(timev-apply [label string?]
                      [#:disp? disp? any/c (*timev?*)]
                      [proc procedure?]
                      [arg any/c] ...)
         any]{
 Similar to @racket[time] but prefixes the displayed time string with @racket[label].

 If @racket[disp?] is @racket[#f], no timing information is displayed.

 Nested calls to @racket[timev-apply] are indented to reflect the nesting.
}

@defform[(timev label [#:disp? (*timev?*)] body ...)]{
A more practical form of @racket[timev-apply].
 @examples[
 #:eval (make-base-eval #:lang 'racket/base '(require timev))
 (timev "Level 0"
        (timev "Level 1a"
               (timev "Level 2"
                      (displayln "Some user output from level 2")))
        (timev "Level 1b"
               (displayln "Some user output from level 1"))
        (displayln "Some user output from level 0"))]
}

@defproc[(*timev?*) boolean? #:value #t]{
 A @racketmodname[global] to make it easy to turn on/off
 @racket[timev] information with a switch from the command line.

 For example:
 @codeblock[#:keep-lang-line? #t]|{
 #lang racket
 (require timev global)

 (*timev?* #f) ; turn timev display off by default

 ;; Use the --timev switch from the command line
 ;; to display timev information
 (void (globals->command-line)) ; parse the command line

 (timev "Calculation time: "
        (+ 1 2))

 (timev "Never print me" #:disp? #f
        (+ 1 2))
 }|

}