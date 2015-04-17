#lang scribble/manual
@(require scribble/eval "util.rkt"
	  (for-label erda/rvm))

@(define the-eval (make-base-eval))
@(the-eval '(require erda/rvm))

@title{@ErdaRkt}

@defmodulelang[erda/rvm]

The @deftech{@ErdaRkt} language is a dynamically typed language that includes an @defterm{alerts} mechanism for declarative error reporting, and transparently propagates errors as data values.

This document describes the syntax and semantics a selection of the @|ErdaRkt|-specific constructs.

@; #%app
@; #%datum
@; quote
@; if
@; if-not
@; or
@; and
@; cond
@; define
@; declare
@; value
@; anti-do
@; try
@; default
@; on-alert
@; block

The @racketmodname[erda/rvm] language also inherits a number of constructs directly from Racket, including the @racket[begin], @racket[begin0], @racket[let], @racket[let*], @racket[letrec], @racket[require], and @racket[provide] syntactic forms, and the @racket[not] function. These forms should therefore behave as described in the Racket documentation. Note, however, that functions may seemingly behave differently due to @ErdaRkt's different function application semantics.

@(close-eval the-eval)
