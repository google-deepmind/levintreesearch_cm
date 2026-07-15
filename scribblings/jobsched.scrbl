#lang scribble/manual
@require[@for-label[jobsched
                    jobsched/remote-call
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
 @item{The server starts @K workers (independent Racket programs).
  Each worker is spawned as a separate OS process and communicates with the server
  over a localhost TCP connection (one per worker).}
 @item{Each worker signals to the server over TCP
  that it is ready to receive jobs.}
 @item{As soon as a worker is ready, the server sends it one of the remaining jobs.}
 @item{When a worker has finished a job, it sends the result to the server, and waits for another job.}
 @item{When the server receives the result of a job, it processes this result and sends a new job (if any is remaining)
 to the worker.}
 @item{When all @N jobs are finished, the server finishes.}]


If your machine has @K cores and sufficient memory, @racketmodname[jobsched] can run @K workers in parallel
and benefit from a speedup of a factor @|K|.
Jobsched has been successfully used with more than 120 workers in parallel,
with more than  100 000 fast-paced jobs to dispatch between them.


@section[#:tag "remote-call"]{Remote-Call: Remote function calls}

@defmodule[jobsched/remote-call]
The bindings in this section are also exported by @racketmodname[jobsched].

The @racketmodname[jobsched/remote-call] module provides a higher-level interface where jobs
are expressed as function calls.

@filebox["server-worker-remote-call.rkt"
         (codeblock
          (file->string (build-path examples "server-worker-remote-call.rkt")))]

Try it with:
@codeblock|{racket -l- jobsched/examples/server-worker-remote-call}|

The @racket[remote-call] macro captures:
@itemlist[
 @item{The function's @bold{module path} (resolved at compile time via @racket[identifier-binding]).}
 @item{The function's @bold{symbol name}.}
 @item{The evaluated @bold{arguments} (positional and keyword).}]

The worker then uses @racket[dynamic-require] to resolve the function and
applies it with the captured arguments. This means the server and worker
are guaranteed to call the same function---eliminating the risk of module
path mismatches.

@bold{Important:} The arguments to the function are @emph{evaluated on the server} before
being serialized and sent to the worker. The worker receives only the function name
and the serialized argument values, and calls the function on these values.
In particular, procedures and closures are @bold{not} serializable
(via @racketmodname[racket/fasl]) and cannot be passed as arguments.
Only the top-level function itself is resolved on the worker via @racket[dynamic-require].

@bold{Remark:} In the example above, the functions, the worker and the server are all defined in the same file.
This is not a requirement, and they can be in three different files.

@defform[(remote-call (fun arg ...))]{
 Creates a remote-call job for the function call @racket[(fun arg ...)].

 At compile time, the macro:
 @itemlist[
  @item{Extracts the module path and symbol for @racket[fun] via @racket[identifier-binding].}
  @item{Raises a compile-time error if @racket[fun] is not @racket[provide]d by any module.}]

We recommend requiring @racketmodname[define2] in the module where the functions are defined,
so as to raise compile-time errors about the function's signature.

 At run time, the arguments are evaluated and stored in the job data,
 but the function is @bold{not} called---it will be called on the worker.

 @bold{Caveats:}
 @itemlist[
  @item{The function must be @racket[provide]d by its module,
   otherwise a compile-time error is raised.}
  @item{All arguments must be serializable via @racketmodname[racket/fasl].
   Procedures, closures, and other non-serializable values cannot be passed as arguments.}
  @item{Renamed imports (e.g., @racket[(require (rename-in racket/list [first my-first]))]) are
   @bold{not} supported. The embedded module path refers to the original name.
   Use re-exported names instead.}
  @item{@tt{planet} modules are untested.}]
}

The @racket[data] argument received by the @racket[#:process-result] callback is
the remote-call value. You can inspect it with the following accessors and predicate:

@defproc*[([(remote-call? [v any/c]) boolean?]
           [(remote-call-fun-sym [rc remote-call?]) symbol?]
           [(remote-call-pos-args [rc remote-call?]) list?]
           [(remote-call-kw-dict [rc remote-call?]) dict?]
           [(remote-call-mod-path [rc remote-call?]) module-path?])]{
 @racket[remote-call?] checks whether a value is a remote-call.
 The accessors return, respectively, the function symbol name, the list of positional arguments,
 the association list of keyword arguments, and the resolved module path.}

@defproc[(start-remote-call-worker) void?]{
 Starts a worker that processes remote-call jobs.
 No arguments are needed---the module path is embedded in each job
 by the @racket[remote-call] macro.

 For each received job, the worker uses @racket[dynamic-require] to load
 the function from the embedded module path, then calls it with the
 stored arguments.}

@section{Lower-level API}

The following sections document the lower-level server/worker API for cases
where @racket[remote-call] does not provide enough control---for example,
when the worker needs to perform custom setup, or when job data is not a
function call.

All definitions exported by the various modules below are also exported by @racketmodname[jobsched].

@section[#:tag "simple server/worker"]{Simple server / worker}

@defproc[(start-simple-worker [run (-> readable? readable?)]
                              [#:silent? silent? any/c #f])
         void?]{
Like @racket[start-worker] except that @racket[run] accepts the data of the job rather
than the job. This masks the @racket[job] object.

Note that @racket[start-simple-worker] can be used with @racket[start-simple-server],
@racket[server-start] and @racket[scheduler-start].

 @bold{IMPORTANT:} See the remarks for @racket[start-worker].
}

@defproc[(start-simple-server [#:worker-file worker-file path-string?]
                              [#:data-list data-list (listof readable?)]
                              [#:process-result process-result (procedure-arity-includes/c 2)]
                              [#:submod-name submod-name symbol? 'worker]
                              [#:n-workers n-workers integer? (min (length data-list) (processor-count))]
                              [#:on-OOM on-OOM (-> scheduler? any) void])
         void?]{

 Creates and starts a server, like @racket[scheduler-start], but hiding the scheduler and the job
 objects.
 The worker is assumed to be in the racket file @racket[worker-file] in the submodule
 @racket[submod-name]. See @racket[start-simple-worker].

 The workers will receive one element of @racket[data-list] at a time, and return
 a result to be processed by @racket[process-result].

 The server starts @racket[n-workers] workers in separate OS processes.
 Refer to @racket[scheduler-start] for @racket[close-workers?].

 Note: By contrast to @racket[scheduler-start], the simple server does not allow to
 add more tasks while it is running.

 @racket[start-simple-server] is essentially a combination of @racket[make-server],
 @racket[server-start] and @racket[server-close].

}

Try the following example with:
@codeblock|{racket -l- jobsched/examples/server-worker-simple}|

@filebox["server-worker-simple.rkt"
         (codeblock
          (file->string (build-path examples "server-worker-simple.rkt")))]

@section[#:tag "server"]{Server}

@defproc[(make-server [#:worker-file worker-file path-string?]
                      [#:submod-name submod-name symbol? 'worker]
                      [#:n-workers n-workers (or/c #f integer?) #f]
                      [#:errortrace? errortrace? any/c]
                      [#:worker-args worker-args (listof (or/c #f string? path?))])
         scheduler?]{
 Returns a new scheduler.
 See @racket[start-simple-server].
 If @racket[n-workers] is not @racket[#f], then the workers are started asynchronously.
 See @racket[make-racket-worker] for the arguments @racket[errortrace?] and @racket[worker-args].
}

@defproc[(server-start [sched scheduler?]
                       [#:data-list data-list (listof readable?)]
                       [#:process-result process-result (procedure-arity-includes/c 2)]
                       [#:n-workers n-workers integer? (min (length data-list) (processor-count))]
                       [#:close-workers? close-workers? any/c #false]
                       [#:on-OOM on-OOM (-> scheduler? any) void])
         void?]{
 Starts an existing scheduler (likely made with @racket[make-server]).
 See @racket[start-simple-server].
 If @racket[close-workers?] is not @racket[#f], then the workers are @emph{not} closed when all jobs
 are done, so that they can be re-used for subsequent calls of @racket[server-start].
}

@defproc[(server-close [sched scheduler?]) void?]{
 Asks all workers of @racket[sched] to exit gracefully, and blocks until all workers have exited.
}

@defproc[(server-force-close [sched scheduler?]) void?]{
 Terminates all workers of @racket[sched] immediately by sending a kill signal to their OS processes.
}

@defproc[(kill-server-on-OOM [sched scheduler?]) void?]{
 An OOM handler that prints an error message to @racket[current-error-port], terminates all workers via @racket[server-force-close], and terminates the server process via @racket[(exit)].
 Useful for passing to @racket[#:on-OOM].
}

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

 The @racket[data] field contains @racket[readable?] (in the sense of @racketmodname[racket/fasl])
 information that is sent to the worker for processing.

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
 A job can be added either before starting the scheduler, or during, using the @racket[#:before-start]
 and @racket[#:after-stop] arguments of @racket[scheduler-start].

 The @racket[data] will be sent to the worker, who will receive it on its input port and will be
 accessible via @racket[job-data].

 The @racket[cost] is used for ordering the job in the priority queue, which is ordered by minimum
 cost.}

@defproc[(scheduler-n-queued-jobs [sched scheduler?]) exact-nonnegative-integer?]{
Returns the number of jobs pending in the queue.}

@defproc[(scheduler-n-active-jobs [sched scheduler?]) exact-nonnegative-integer?]{
Returns the number of jobs that have started and not yet finished.}

@defproc[(scheduler-start [sched scheduler?]
                          [n-workers (or/c #f exact-nonnegative-integer?) #f]
                          [#:before-start before-start (-> scheduler? job? any) void]
                          [#:after-stop after-stop (-> scheduler? job? readable? any) void]
                          [#:close-workers? close-workers? any/c #t]
                          [#:on-OOM on-OOM (-> scheduler? any) void])
         void?]{
Starts a scheduler,
making sure that @racket[n-workers] racket worker instances are running on the same machine.

If @racket[close-workers?] is not @racket[#f], then all worker instances are closed when
 returning from the function call, and the workers normally terminate gracefully.
If @racket[close-workers?] is @racket[#f], workers are not closed when returning from the call,
so they can be re-used for a subsequent call to @racket[scheduler-start] without starting the
workers (and the corresponding racket instances) again.
 If @racket[n-workers] is @racket[#f], the number of running worker instances is not changed,
 and previous running instances are re-used.
 If there are already more running worker instances than @racket[n-workers], some workers are
 killed to match @racket[n-workers].

The callback @racket[before-start] is called before a job is sent to a worker.
The callback @racket[after-stop] is called when a job is finished and the result is received
from the worker.
Both callbacks can be used to add new jobs to the queue, using @racket[scheduler-add-job!].

If @racket[on-OOM] is not @racket[void], a memory guard thread is started (Linux only) which monitors system memory.
If system memory is low, it calls @racket[(on-OOM sched)].
A useful handler is @racket[kill-server-on-OOM] which prints an error, kills workers, and exits.

See an example of using @racket[scheduler-start] in @tt{examples/server-worker}.
}

@defproc[(processor-count) nonnegative-integer?]{
Re-exported from @racketmodname[racket/future].}

@section{Worker}

@defmodule[jobsched/worker]
The bindings in this section are also exported by @racketmodname[jobsched].

@defproc[(start-worker [run-job (-> job? readable?)]
                       [#:silent? silent? any/c #f]) void?]{
 Starts a worker which waits for jobs.
 Each time a job is received, the @racket[run-job] procedure is called.
 The data of the job can be retrieved with @racket[(job-data job)].
 If @racket[silent?] is not @racket[#f], all output of @racket[run-job] to its output port is
 suppressed---the error port remains untouched.

 See example at the top.

 Communication between server and worker uses TCP on localhost.
 The worker's stdout/stderr are redirected to the server's stderr,
 so @racket[printf] and @racket[displayln] calls in the worker are visible
 for debugging and do @bold{not} interfere with the protocol.

 @racket[run-job] must return a @racket[readable?] value (in the sense of @racketmodname[racket/fasl]),
 and @racket[(void)] is not readable.
}

@section[#:tag "utils"]{Utilities}

@defmodule[jobsched/utils]
The bindings in this section are also exported by @racketmodname[jobsched].

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

@defform[(this-file)]{
 'Returns' the path-string of the enclosing file, or @racket[#f] if there is no enclosing file.}


@defproc[(start-memory-guard-thread [#:on-OOM on-OOM (-> any) (λ () (eprintf "OUT OF MEMORY\n") (exit))]
                                    [#:OOM-ratio OOM-ratio real? 0.05]
                                    [#:wait-seconds wait-seconds real? 10])
         (or/c thread? #false)]{
 Starts a thread that monitors system memory and calls @racket[on-OOM] when available memory drops below a certain ratio.

 This function is Linux-specific (it reads @filepath{/proc/meminfo}). On other platforms, it returns @racket[#false] immediately without starting a thread.

 The @racket[on-OOM] callback should be a thunk. By default it prints @verbatim{OUT OF MEMORY} to stderr and exits.
 Note that if called from a server, this default handler will @bold{not} kill worker processes! For server OOM handling, use the @racket[#:on-OOM] argument of @racket[server-start] or @racket[scheduler-start] instead, or use @racket[server-force-close] inside the callback.

 The @racket[OOM-ratio] is the ratio of available memory to total memory. If available memory is less than @racket[OOM-ratio] * total memory, OOM is triggered. Default is @racket[0.05] (5%).

 The query is performed every @racket[wait-seconds] seconds.
}


@section[#:tag "comparison"]{Comparison with other Racket parallelism mechanisms}

@secref["futures" #:doc '(lib "scribblings/reference/reference.scrbl")] can make use of
many cores and can share memory, but are limited in the kind of operations they can handle without
blocking.
Futures are well suited for numerical applications where garbage collection can be greatly reduced,
allowing to make the most of the CPUs without requiring multiple Racket instances.


@secref["places" #:doc '(lib "scribblings/reference/reference.scrbl")] allow more free-form racket
computation and can make use of multiple cores and can share memory,
but are still constrained by a single garbage collector,
which prevents from making full use of all the cores.

 @other-doc['(lib "scribblings/distributed-places/distributed-places.scrbl")]
create separate Racket instances like @racketmodname[jobsched] but can also spawn workers on remote
machines.
When all workers are on the same machine, @racketmodname[jobsched] may be simpler to use.
