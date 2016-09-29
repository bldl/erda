#lang scribble/manual
@(require scribble/eval "util.rkt"
  (for-label erda/ga erda/ga/contract
    (only-in racket/contract -> any/c none/c or/c and/c)
    (prefix-in rkt. racket/base)
    ))

@(define the-eval (make-base-eval))
@(the-eval '(require erda/ga
              (only-in erda/i3-internal print-Bad-concisely?)))
@(interaction-eval #:eval the-eval (print-Bad-concisely? #t))

@title{@ErdaGa}

@defmodulelang[erda/ga]

The @deftech[#:key "ErdaGa" ErdaGa] language is a dynamically typed functional language such that it includes an alerts mechanism for declarative error reporting, and transparently propagates errors as data values. A particular aim for @ErdaGa is to adhere to the semantics of guarded algebras.

@ErdaGa is superficially very similar to @tech[#:key "ErdaRkt" ErdaRkt], but it has some notable differences in both its design and implementation:
@itemlist[

@item{In @ErdaGa functions are (wrapped) values, and hence may be passed around as such, enabling the definition of @|ErdaGa|-native higher-order functions. An attempt to apply a bad function (or a non-function) will naturally fail, resulting in a bad value.}

@item{@ErdaGa emits the necessary glue code for alert processing into function definition sites rather than call sites. Consequently, it is possible to support alerts also for anonymous functions.}

@item{In @ErdaGa, @emph{all} function arguments are evaluated before determining whether they are good arguments for the function. @|ErdaRkt|'s argument evaluation is less eager.}

@item{@ErdaGa records failed expression history in a manner that allows for ``replay'' of failed operations, using @racket[redo] and related functions.}

]

This document describes the syntax and semantics a selection of those @|ErdaGa| constructs that have notable differences to @ErdaRkt's. Overall, @ErdaGa's constructs generally have the same syntax and semantics as those of @ErdaRkt, and we do not document them separately here.

@defform*[((declare (id arg ...) #:is tgt-id #:direct)
           (declare (id arg ...) #:is tgt-id maybe-alerts))]{
Forms used to wrap a primitive target function @racket[tgt-id] into a thunk that does error processing, and which is then bound as @racket[id].

The significant difference between @ErdaGa and @ErdaRkt is that since in @ErdaRkt error processing code is emitted into call sites, the same function can be applied in two modes, directly or with implicit error processing, with any alerts specified with @ErdaRkt-racket[declare]. In @ErdaGa, on the other hand, ``direct'' and ``Erda'' functions have different bindings, and thus @racket[let-direct] and related forms do not alter the error processing behavior of applications of @racket[declare]d functions.}

@defform*[((lambda args #:direct body ...+)
           (lambda args #:handler maybe-alerts body ...+)
	   (lambda (arg ...) #:primitive maybe-alerts body ...+)
           (lambda args maybe-alerts body ...+))
	   #:grammar
	   ([args (arg ...) (arg ... . rest-id) rest-id])
	   ]{
An anonymous function expression.

A @racket[#:direct] function must have a @racket[body] that is prepared to handle wrapped (good or bad) argument values, and will produce a wrapped value.

A @racket[#:handler] is not protected from receiving bad values as arguments, unless alerts are specified to guard against that.

The body of a @racket[#:primitive lambda] is protected against bad arguments, and it is assumed that the body processes bare values, and produces a bare value. Any alerts that are given must be in terms of wrapped values, however.

The ``regular'' @racket[lambda] form produces a function that processes wrapped values, but its @racket[body] can expect to see no bad arguments.}	   

@defform[(thunk rest ...+)]{
Like @racket[lambda] without parameters.}

@defproc[(apply [fun Result?] [args Result?]) Result?]{
Applies the specified @racket[fun]ction on the specified list of (wrapped) arguments @racket[args].
(See the @racketidfont{args} functions for manipulating such lists.)}

@defproc[(function? [x any/c]) (Good/c rkt.boolean?)]{
A predicate recognizing values that may be applied, e.g., with @racket[apply].

For example:
@(interaction #:eval the-eval
  (function? function?))}

@defform[(let-direct ([arg expr] ...) body ...+)]{
Behaves like @ErdaRkt-racket[let-direct], except that the definition of function application is not altered as radically as for @|ErdaRkt|.}

@defform[(direct-lambda (arg ...) maybe-alerts body ...+)]{
Similar to @racket[let-direct], but wraps the direct code into an anonymous function, for which alerts may also be specified.}

@defform[(define-direct (id arg ...) maybe-alerts body ...+)]{
A combination of @racket[define] and @racket[direct-lambda].}

@defproc[(if-then [cond-value Result?]
                  [then-thunk (Result/c rkt.procedure?)]
		  [else-thunk (Result/c rkt.procedure?)]) Result?]{
A handler function used in translation of @racket[if] forms.
The @racket[cond-value] determines which thunk gets applied, and it is acceptable for the unapplied one to be bad.
This function is not meant to be used directly, but it may be useful to know it, in order to be able to compare function values when inspecting history.}

@defform[(cond cond-clause ... else-clause)
	 #:grammar ([cond-clause (test-expr then-expr ...+)]
	            [else-clause (#:else then-expr ...+)])]{
A conditional expression that processes wrapped values. If any of the @racket[test-expr]essions yields a bad value, none of the remaining expressions are evaluated, and the overall result will also be bad. Note the compulsory @racket[#:else] clause, which is a significant difference compared to @Racket-racket[cond].

For example:
@(interaction #:eval the-eval
  (::> (cond
         [#f 'false]
         [(raise 'bad) 'bad]
         [#:else 'otherwise])
       'cond-was-bad))}

@defproc[(>>= [v Result?] [f Result?]) Result?]{
An identity-monadic bind, such that @racket[v] is passed directly onto the function @racket[f] if both are good arguments for @racket[>>=]. The argument @racket[f] must be a function callable with one argument.

For example:
@(interaction #:eval the-eval
  (>>= #f not))}

@defform[#:literals (<-)
  (do sub-do ... expr)
  #:grammar
  ((sub-do [x-id <- expr] expr))]{
A ``monadic'' expression that uses @racket[>>=] as its monadic ``bind'' function, but gets its error-monadic semantics from the @|ErdaGa| language.
Sequencing stops once an @racket[expr]ession produces a bad value.
Naturally, as Erdas process bad values throughout, any good first argument for @racket[>>=] is not unwrapped, as a Haskeller might expect.

For example:
@(interaction #:eval the-eval
  (define bad (raise 'bad))
  (do 42)
  (do bad 42)
  (do [x <- bad] 42))}

@defproc[(not [v Result?]) Result?]{
Like @Racket-racket[not], but extended to process wrapped values.}

There are still more commonalities in the two languages' syntax and standard library:
@itemlist[
@item{@ErdaGa-like-ErdaRkt[define #t].}
@item{@ErdaGa-like-ErdaRkt[::> #t].}
@item{@ErdaGa-like-ErdaRkt[try #t].}
@item{@ErdaGa-like-ErdaRkt[on-alert #t].}
@item{@ErdaGa-like-ErdaRkt[if #t].}
@item{@ErdaGa-like-ErdaRkt[and #t].}
@item{@ErdaGa-like-ErdaRkt[or #t].}
@item{@ErdaGa-like-ErdaRkt[raise #f].}
@item{@ErdaGa-like-ErdaRkt[raise-with-cause #f].}
]

@section{Inspecting and Using History}

@ErdaGa has @defidentifier[#'result?], @defidentifier[#'good-result?], and @defidentifier[#'bad-result?] predicates, which are like @|ErdaRkt|'s
@racket/ErdaRkt[result?], @racket/ErdaRkt[good-result?], and @racket/ErdaRkt[bad-result?].

For inspecting the contents of bad values, @ErdaGa has functions such as @racket[bad-result-alert-name] (which is also in @ErdaRkt), @racket[bad-result-fun], and @racket[bad-result-args].

@defproc[(bad-result-alert-name [v Result?]) (Result/c rkt.symbol?)]{
Returns the alert name of @racket[v] if it is a bad result.
Otherwise returns a bad-argument error.}

@defproc[(bad-result-fun [v Result?]) Result?]{
Returns the value whose application yielded the bad result @racket[v].
(Such a value may not necessarily be a function, which would have caused a failure to apply it.)
Returns a bad-argument error if @racket[v] is not a bad result.}

@defproc[(bad-result-args [v Result?]) (Result/c rkt.list?)]{
Returns the arguments of the failed operation that produced the bad result @racket[v], as an argument list containing wrapped values.
(See the @racketidfont{args} functions for manipulating such lists.)
Returns a bad-argument error if @racket[v] is not a bad result.}

For argument list manipulation @|ErdaGa|'s standard library includes the functions @defidentifier[#'args-list?], @defidentifier[#'args-list], @defidentifier[#'args-cons], @defidentifier[#'args-car], @defidentifier[#'args-cdr], and @defidentifier[#'args-list-set], which are similar to the Racket equivalents, but defined as handlers as necessary to be able to deal with bad values contained in argument lists.

@ErdaGa is also able to ``replay'' failed operations with the @racket[redo], @racket[redo-apply], and @racket[redo-app] functions, all of which are handlers so that they can accept a bad result as their first argument. The latter two @racketidfont{redo} functions make it possible to invoke the failed operation with different arguments.

@defproc[(redo [v Result?]) Result?]{
Replays the failed operation that produced the result @racket[v].
It will naturally fail again with the same result unless it was dependent on state.

For example:
@(interaction #:eval the-eval
  (redo bad))}

@defproc[(redo-apply [v Result?] [args Result?]) Result?]{
Replays the failed operation that produced the result @racket[v], but using the specified argument list @racket[args].

For example:
@(interaction #:eval the-eval
  (redo-apply bad (args-list 'worse)))}

@defproc[(redo-app [v Result?] [arg Result?] ...) Result?]{
Replays the failed operation that produced the result @racket[v], but using the specified argument list @racket[arg ...].

For example:
@(interaction #:eval the-eval
  (redo-app bad 'still-worse))}

@section{Contracts}

@defmodule[erda/ga/contract]

In documenting @ErdaGa functions, we state @tech[#:doc '(lib "scribblings/reference/reference.scrbl")]{contracts} for them as is usual for Racket documentation. These are stated so that they account for any bad-argument extension of the function, and are explicit about wrapped values. It is a @emph{programming error} to call a function with arguments that do not adhere to the contract.

We specify the contracts for function arguments and results using predicates and predicate combinators, some of which come from Racket. Where we want to be explicit about our use of Racket primitives, we prefix them with @racketidfont{rkt.}.

@|ErdaGa| also has an internal API that includes some of its own predicates such as @racket[Result?], @racket[Good?], and @racket[Bad?], and some  predicate combinators, which have a @racketidfont{/c} in their name, as is customary for contracts in Racket. These are only intended for internal use, documentation purposes, and perhaps for actually specifying enforced contracts.

We are presently not documenting alert declarations, which are different in their role. In particular, any bad conditions covered by declared alerts cannot be a programming error, since cases are handled implicitly.

@defproc[(Result? [x any/c]) rkt.boolean?]{
A predicate that recognize's @|ErdaGa|'s wrapped values.}

@defproc[(Good? [x any/c]) rkt.boolean?]{
A predicate that recognize's @|ErdaGa|'s good wrapped values.}

@defproc[(Bad? [x any/c]) rkt.boolean?]{
A predicate that recognize's @|ErdaGa|'s bad wrapped values.}

@deftogether[
  (@defproc[(Result/c [p? rkt.procedure?]) (-> any/c rkt.boolean?)]
   @defproc[(Good/c [p? rkt.procedure?]) (-> any/c rkt.boolean?)])]{
For building predicates on wrapped values, in terms of a @emph{primitive} predicate that is passed in as an argument.

That is, calling @racket[Result/c] or @racket[Good/c] instantiates a Racket predicate which applies the specified Racket primitive predicate @racket[p?] on any bare value inside a good value.

The difference between the two functions is that @racket[Result/c] produces predicates that return @racket[#t] for bad values, while @racket[Good/c]'s predicates return @racket[#f] for bad values.}

@(close-eval the-eval)
