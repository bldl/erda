#lang scribble/manual
@;;;###action ((master-file . "erda.scrbl") (quiet . t))
@(require scribble/eval "util.rkt"
	  (for-label (only-in racket/contract any/c) erda/rvm))

@(define the-eval (make-base-eval))
@(the-eval '(require erda/rvm))

@title{@ErdaRkt}

@defmodulelang[erda/rvm
  #:use-sources (erda/i1-surface erda/i1-lib)]

The @deftech{@ErdaRkt} language is a dynamically typed language that includes an @defterm{alerts} mechanism for declarative error reporting, and transparently propagates errors as data values.

This document describes the syntax and semantics of a selection of the @|ErdaRkt|-specific constructs.

The @racketmodname[erda/rvm] language also inherits a number of constructs directly from Racket, including the @racket[begin], @racket[begin0], @racket[let], @racket[let*], @racket[letrec], @racket[require], and @racket[provide] syntactic forms, and the @racket[not] function. These forms should therefore behave as described in the Racket documentation. Note, however, that functions may seemingly behave differently due to @ErdaRkt's different function application semantics.

@section{Modules and Macros}

The Racket @racket[require] and @racket[provide] forms (and associated sub-forms) may be used in Erda as normal to import modules and to define the interfaces exposed by modules.

Macros are not included in the language by default, but there is nothing preventing from @racket[require]'ing macro support from Racket.

@section{Defining Forms}

@defform*[((define id expr)
	   (define (id arg ...) maybe-alerts expr ...+)
	   (define (id arg ...) #:handler maybe-alerts expr ...+)
	   (define (id arg ...) #:direct expr ...+))]{
Forms used to define variables and functions.

The semantics of the @racket[(define id expr)] form is the same as in Racket. In @ErdaRkt functions are not first class, and the language does not include a @racketid[lambda] form, and thus this form is intended only for defining variables (that do not name functions).

The second form, which is for defining ``regular'' @ErdaRkt functions, binds @racket[id] as a function that takes arguments @racket[arg ...]. The language enforces that the arguments will all have to be good, wrapped values (i.e., values for which the predicate @racket[good-result?] holds); there is an implicit alert guarding against bad values. Explicit alerts may be specified according to the @racket[maybe-alerts] grammar. The result of the function should be a single wrapped value (i.e., values for which @racket[result?] holds). Indeed, @ErdaRkt does not support multi-value returns, or more generally, multi-value expressions. The language enforces, on the single return value, the data invariant associated with its type; the function application will produce a bad value instead if the invariant does not hold. Explicit post-conditions are treated similarly. The invariants are not checked on bad results.

The @racket[#:handler] variant of the @racket[define] form is like the ``regular'' function definition form, but without the implicit alerts requiring good arguments, or the assumption that post-condition expressions require good free variables. That is, any pre-conditions get evaluated even if some of the arguments are bad, and if they hold, the function gets called. Similarly, any post-conditions (but not data invariant) are also checked on a bad result. The intention is for this kind of function definition to make it possible to implement ``handler'' functions able to process (and perhaps recover from) bad values.

The @racket[#:direct] variant of the form defines a function that is called directly, without the language doing any pre- or post-processing on the incoming or outgoing values, which are still expected to be wrapped (no data invariants is checked either, so beware). This form of @racket[define] is intended to allow for the implementation of @ErdaRkt functions (such that they are aware of wrapped values) as Racket-based primitives, probably using @Racket-racket[#%plain-app] form as the ``FFI'' for implementing such primitives within the @|ErdaRkt| language. Perhaps more likely, you'll want to implement such functions in Racket, and instead merely @racket[declare] them as @racket[#:direct].}

@defform*[((declare (id arg ...) maybe-alerts)
	   (declare (id arg ...) #:direct))]{
Forms used to specify information about functions, not to implement them, or to bind the identifier @racket[id]. The binding must already exist.

The first @racket[declare] form declares a Racket primitive that processes unwrapped values, and thus will get automatic unwrapping/wrapping at the application site. Alert clauses may be specified, with any @racket[test-expr] evaluated with the @racket[arg] (and @racket[value], as appropriate) identifiers bound to @emph{wrapped} values; in other words, despite a primitive function being called, the conditional expressions are still written in @|ErdaRkt|. As many existing Racket functions may throw exceptions, it is quite important to specify @racketid[on-throw] alert clauses as appropriate, as a way of converting from such a foreign error reporting mechanism.

The second @racket[declare] form is like the @racket[#:direct] @racket[define] form, but without taking an implementation. An implementation must already be bound as the function @racket[id].

It is also possible to call un@racket[declare]d Racket functions, as long as they are bound. Naturally, then, no explicit alerts have been specified, but goodness of arguments is nonetheless enforced, and arguments are unwrapped automatically; undeclared functions are expected to process unwrapped values. There is no catching of exceptions, but the data invariant of the result value is checked. A broken DI leads to the result being automatically wrapped as a bad value, whereas otherwise it is wrapped as a good value.}

@subsection{Alerts}

The alert specification of a @racket[define]d or @racket[declare]d function matches the following grammar.

@racketgrammar*[
#:literals (pre-when pre-unless post-when post-unless on-throw)
[maybe-alerts code:blank (code:line #:alert (alert-clause ...))]
[alert-clause
  (alert-id pre-when test-expr)
  (alert-id pre-unless test-expr)
  (alert-id post-when test-expr)
  (alert-id post-unless test-expr)
  (alert-id on-throw pred-expr)]]

where:

@specsubform[alert-id]{
An alert name. The name of the alert to trigger if the corresponding @racket[test-expr] holds, or if the corresponding @racket[pred-expr] predicate holds for a thrown exception object.}

@specsubform[test-expr]{
An @ErdaRkt expression, computing in wrapped values.
The @racket[test-expr]s of @racket[#:handler] functions should probably be able to deal with bad values as well.

The expression is automatically negated for the @racketid[pre-unless] and @racketid[post-unless] cases.

For post-condition expressions, the result of the function application is bound as @racket[value].}

@specsubform[pred-expr]{
A Racket predicate expression, computing in bare values. The predicate should accept any bare @racket/Racket[exn] structure (or a subtype) as an argument, and yield a bare value indicating whether the predicate holds.
}

@section{Expressions}

@defform[(#%datum . datum)]{
A good literal value, as specified by @racket[datum].

For example:
@(interaction #:eval the-eval 0)}

@defform[(quote id)]{
A good symbol @racketvalfont{'}@racketvarfont{id}. In @ErdaRkt symbols are primarily used to name alerts.

For example:
@(interaction #:eval the-eval 'not-found)}

@defidform[value]{
A result value. Bound in the scope of a @racket[define] or @racket[declare] post-condition expression, for example, but also in some other syntactic contexts that have expressions for result processing (see @racket[try], for example).
}

@defform[(if test-expr then-expr else-expr)]{
Like @Racket-racket[if], but processes wrapped values. If @racket[test-expr] yields a bad value, then the overall expression yields a bad value, and neither branch is evaluated. If @racket[test-expr] yields a good @racket[#f], then the @racket[else-expr] expression is evaluated. Otherwise @racket[then-expr] is evaluated.

For example:
@(interaction #:eval the-eval
  (if (raise 'bad) 'yes 'no))}

@defform[(if-not test-expr then-expr else-expr)]{
Equivalent to @racket[(if test-expr else-expr then-expr)].
}

@defform[(or expr ...)]{
Like @Racket-racket[or], but processes wrapped values. If any of the @racket[expr]essions yields a bad value, none of the remaining expressions are evaluated, and the overall result will also be bad.
}

@defform[(and expr ...)]{
Like @Racket-racket[and], but processes wrapped values. If any of the @racket[expr]essions yields a bad value, none of the remaining expressions are evaluated, and the overall result will also be bad.
}

@defform[#:literals (else)
         (cond cond-clause ... else-clause)
	 #:grammar ([cond-clause (test-expr then-expr)]
	            [else-clause (else then-expr)])]{
A conditional expression that processes wrapped values. If any of the @racket[test-expr]essions yields a bad value, none of the remaining expressions are evaluated, and the overall result will also be bad. Note the compulsory @racket[else] clause, which is a significant difference compared to @Racket-racket[cond].

For example:
@(interaction #:eval the-eval
  (cond
    [#f 'false]
    [(raise 'bad) 'bad]
    [else 'otherwise]))}

@defform[(let-direct ([arg expr] ...) body ...+)]{
Locally switches to a bare-value ``evaluation mode,'' for the @racket[body] expressions. Each argument value, given by @racket[expr], is unwrapped and bound to the corresponding @racket[arg], which must be an identifier. Said identifiers will be bound in the scope of the body. The result of the body expressions is then again wrapped, in either a good or bad wrapper, based on the DI. The overall expression fails if any @racket[expr] yields a bad value, and in that case the body is left unevaluated.
}

@defform[#:literals (_)
  (try body ...+ #:catch catch-clause maybe-catch-all)
  #:grammar ([catch-clause ((id ...) then-expr ...+)]
             [maybe-catch-all code:blank (_ then-expr ...+)])]{
Evaluates the @racket[body] expressions, and if the last of them yields a bad result, then matches it against the @racket[catch-clause]s based on the alert name of the bad value. The optional @racket[maybe-catch-all] clause will match anything. If the body result is good, or if there is no matching clause, then that result remains the result of the overall expression. Otherwise the result is given by the last @racket[then-expr] of the first matching clause.

For example:
@(interaction #:eval the-eval
  (try (raise 'worst)
   #:catch [(bad worse still-worse) 1]
           [(worst) 2]
	   [_ 3])
  (try (raise 'bad) #:catch [(worse worst) 3])
  (try 1 #:catch [_ 3])
  (try (raise 'bad) #:catch [_ 3]))
}

@defform[(::> try-expr ... fail-expr)]{
Evaluates the @racket[try-expr] expressions in order, until one of them yields a good value, which then becomes the value of the overall expression. Where no @racket[try-expr] evaluates to a good value, the result of the overall expression is that of @racket[fail-expr].

For example:
@(interaction #:eval the-eval
  (::> 'good 'alternative)
  (::> (raise 'bad) 'alternative)
  (::> (raise 'bad) (raise 'worse) 'alternative)
  (::> (raise 'bad) (::> (raise 'nested-bad)) (raise 'no-good)))}

@defform[(on-alert (handler-clause ...) body ...+)
#:grammar ([handler-clause ((id ...) expr ...+)])]{
Installs handlers for the scope of the @racket[body] expressions, the last of which normally gives the result of the overall expression.

If any application of a function listed by @racket[id] fails (with a bad result), then a matching clause's @racket[expr]essions are evaluated, and the result of the last of them is substituted in place of the result of the failed function call.

This recovery mechanism does not apply to syntactic forms (even if named by @racket[id]), nor will recovery happen within the body of an @racket[let-direct] expression.

For example:
@(interaction #:eval the-eval
  (on-alert () 'nothing)
  (on-alert ([(not) 'good]) (raise 'bad))
  (on-alert ([(raise) 'good]) (raise 'bad)))
}

@defform[(block stat ... result-expr)
#:grammar ([stat (#:let id expr)
                 (#:when test-expr #:let id expr)
		 expr])]{
Evaluates a sequence of restricted ``statements,'' in the order given. Each @racket[stat] may be an assignment, a conditional assignment, or an expression. Conditional assignment only happens if the condition is a good true value. What appears to be assignment to a previously defined variable is actually a shadowing single static assignment. Any @racket[id] that gets bound is in scope for the rest of the expression. A restriction of conditional assignment is that conditional assignment to an unbound @racket[id] is not allowed, as then @racket[id] might not be bound for the rest of the expression.

The evaluation of the overall expression immediately stops with a bad value if a @racket[test-expr] produces a bad value.  Where there were no failures in conditionals, the overall result of the expression will be that of @racket[result-expr].

For example:
@(interaction #:eval the-eval
  (block 1 2 3)
  (block [#:let x 1] x)
  (block [#:let x 1] [#:let x 2] x)
  (block [#:let x 1] [#:when #t #:let x 2] x)
  (block [#:let x 1] [#:when #f #:let x 2] x)
  (block [#:let x (raise 'bad)] [#:when x #:let x 'good] x))
}

@section{Standard Library}

This section lists a small selection of the @ErdaRkt standard library.

The documented argument and result types (or predicates, rather) are only for informational purposes; they are not necessarily enforced using actual contracts (indeed the language does not have support for contracts built-in).

Some functions do have pre- and post-conditions specified with alert clauses, but these are not indicated in the signatures shown here; the signatures here reflect the functions' own ability to handle inputs. It is the @ErdaRkt language itself that does further enforcing, according to explicit or implicit alert conditions.

@defproc[(result? [x any/c]) good-result?]{
A predicate that holds if @racket[x] is a wrapped value (whether good or bad). The result of the predicate is itself wrapped.}

@defproc[(good-result? [x any/c]) good-result?]{
A predicate that holds if @racket[x] is a good (wrapped) value.}

@defproc[(bad-result? [x any/c]) good-result?]{
A predicate that holds if @racket[x] is a bad (wrapped) value.

For example:
@(interaction #:eval the-eval
  (bad-result? (raise 'worst)))}

@defproc[(raise [alert-name good-result?]) bad-result?]{
Creates a new bad value with the specified @racket[alert-name], passed in as a wrapped symbol. The constructed bad value will have no history beyond the call to this function.}

@defproc[(raise-with-cause [alert-name good-result?] [cause bad-result?]) bad-result?]{
Creates a new bad value with the specified @racket[alert-name] and the specified @racket[cause], where @racket[cause] should be a badness that triggered the error being raised. The constructed bad value will have @racket[cause] as a separate ``branch'' of history.

For example:
@(interaction #:eval the-eval
  (raise-with-cause 'follow-up (raise 'cause)))}

@(close-eval the-eval)
