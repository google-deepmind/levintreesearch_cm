#lang scribble/manual
@require[@for-label[jobsched
                    data/heap
                    racket/contract/base
                    racket/future
                    racket/base]
         racket/runtime-path
         racket/string
         racket/list
         racket/file
         racket/contract/base]

@title{Jobsched: Running jobs with multiple Racket instances}
@author{Laurent Orseau}

@(define K @emph{K}#;|number of workers|)
@(define N @emph{N}#;|number of jobs|)
@(define-runtime-path examples "../examples")

@defmodule[jobsched]

Jobsched is a job scheduler with a server-worker architecture on a single machine,
where the server dispatches a number of jobs to the workers.

All workers are identical and are able to process any job assigned by the server.
The workers are independent racket programs, each with its own Racket instance.
See @Secref["comparison"] for more information.


The workflow is as follows:
@itemlist[#:style 'ordered
 @item{The server (a Racket program) is started.}
 @item{@N jobs are added to the job queue in the server. A job is specified as @racket[read]-able data.}
 @item{The server starts @K workers (independent Racket programs).}
 @item{Each worker signals on their output ports
  to the server that they are ready to receive jobs,
           and listens to events on its input port.}
 @item{As soon as a worker is ready, the server sends it one of the remaining jobs.}
 @item{When a worker has finished a job, it sends the result to the server, and waits for another job.}
 @item{When the server receives the result of a job, it processes this result and sends a new job (if any is remaining)
 to the worker.}
 @item{When all @N jobs are finished, the server finishes.}]


If your machine has @K cores and sufficient memory, @racketmodname[jobsched] can run @K workers in parallel
and benefit from a speedup of a factor @|K|.
Jobsched has been successfully used with more than 120 workers in parallel,
with more than  100 000 fast-paced jobs to dispatch between them.


@section{Example}

Here is a simple example of a server-worker architecture.
The first file contains the definition of the server:
@filebox["adder-server.rkt"
         (codeblock
          (string-join
           #:before-first "#lang racket"
           (drop (file->lines (build-path examples "adder-server.rkt"))
                 14)
           "\n"))]
And the second file contains the definition of the worker:
@filebox["adder-worker.rkt"
         (codeblock
          (string-join
           #:before-first "#lang racket"
           (drop (file->lines (build-path examples "adder-worker.rkt"))
                 14)
           "\n"))]


All definitions exported by the various modules below are also exported by @racketmodname[jobsched].

See also the one-file example in @tt{examples/server-worker}.


@section{Job}

@;{TODO: Define readable}
@(define readable? #f)

@defmodule[jobsched/job]
The bindings in this section are also exported by @racketmodname[jobsched].


@defstruct*[job ([index nonnegative-integer?]
                 [cost number?]
                 [data readable?]
                 [start-ms nonnegative-integer?]
                 [stop-ms nonnegative-integer?])
            #:transparent]{
 This structure is usually used only for accessing information by a worker or by the server.

 The @racket[cost] field is used by the priority queue of the server.

 The @racket[data] field contains @racket[readable?] information that is sent to the worker
 for processing.

 The fields @racket[start-ms] and @racket[stop-ms] are set automatically by the server
 when a job is sent to a worker and when the result is received.}

@section{Server}

@defmodule[jobsched/server]
The bindings in this section are also exported by @racketmodname[jobsched].

@defproc[(scheduler? [v any/c]) boolean?]{}
@defproc[(worker? [v any/c]) boolean?]{}

@defproc[(make-scheduler [make-worker-command (-> nonnegative-integer? list?)]) scheduler?]{
 Returns a scheduler which will use @racket[make-worker-command] to start the workers'
 Racket processes.

See also @racket[make-racket-cmd].}

@defproc[(scheduler-add-job! [sched scheduler?] [#:data data readable?] [#:cost cost number? 0])
         void?]{
 Adds a job to the scheduler's queue.

 The @racket[data] will be sent to the worker, who will receive it on its input port and will be
 accessible via @racket[job-data].

 The @racket[cost] is used for ordering the job in the priority queue, which is ordered by minimum
 cost.}

@defproc[(scheduler-start [sched scheduler?]
                          [n-workers nonnegative-integer?]
                          [#:before-start before-start (-> scheduler? job? any) void]
                          [#:after-stop after-stop (-> scheduler? job? readable? any) void])
         void?]{
Starts a scheduler. @racket[n-workers] racket instances are started on the same machine.

The callback @racket[before-start] is called before a job is sent to a worker.
This can be used to add new jobs to the scheduler.
@;emph{TODO: we could use this to filter out the job, but needs to loop}

The callback @racket[after-stop] is called when a job is finished and the result is received
from the worker.
This can be used to process the result and add jobs to the queue.
}

@defproc[(processor-count) nonnegative-integer?]{
Re-exported from @racketmodname[racket/future].}

@section{Worker}

@defmodule[jobsched/worker]
The bindings in this section are also exported by @racketmodname[jobsched].

@defproc[(start-worker [run-job (-> job? any)]
                       [silent? (#:silent? any/c #f)]) void?]{
 Starts a worker which waits for jobs.
 Each time a job is received, the @racket[run-job] procedure is called.
 The data of the job can be retrieved with @racket[(job-data job)].
 If @racket[silent?] is not @racket[#f], all output of @racket[run-job] to its output port is
 suppressed---the error port remains untouched.

 See example at the top.

 NOTICE: Any output @emph{before} @racket[start-worker] is called is processed by the server,
 who is waiting for a ready signal from the worker. It is advised to avoid any output before
 @racket[start-worker].
}

@section[#:tag "utils"]{Utilities}

@defmodule[jobsched/utils]

@defproc[(make-racket-cmd [path-to-prog path-string?]
                          [#:submod submod (or/c symbol? #f) #f]
                          [#:errortrace? errortrace? any/c #f]
                          [args path-string?]
                          ...)
         (listof path-string?)]{
Creates a command line to call the racket program @racket[path-to-prog].
 If @racket[submod] is specificied, the corresponding submodule is called instead.
 (For example I like to use a @racket[worker] submodule.)
 By default, the @racket[main] submodule is used if available, or the @racket[main] function
 if available.
 The additional command-line arguments @racket[args] are passed to the program,
 which may choose to parse them.
 Note that @racket[path?] arguments are turned automatically into strings by Racket's primitives.
}



@section[#:tag "comparison"]{Comparison with other Racket parallelism mechanisms}

@secref["futures" #:doc '(lib "scribblings/reference/reference.scrbl")] can make use of
many cores and can share memory, but are limited in the kind of operations they can handle without
blocking.
Futures are well suited for numerical applications where garbage collection can be greatly reduced,
allowing to make the most of the CPUs without requiring .


@secref["places" #:doc '(lib "scribblings/reference/reference.scrbl")] allow more free-form racket
computation and can make use of multiple cores and can share memory,
but are still constrained by a single garbage collector,
which prevents from making full use of all the cores.

 @other-doc['(lib "scribblings/distributed-places/distributed-places.scrbl")]
create separate Racket instances like @racketmodname[jobsched] but can also spawn workers on remote
machines.
When all workers are on the same machine, @racketmodname[jobsched] may be simpler to use.
