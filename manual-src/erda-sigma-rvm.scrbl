#lang scribble/manual
@;;;###action ((master-file . "erda.scrbl") (quiet . t))
@(require scribble/eval "util.rkt"
	  (for-label erda/sigma-rvm))

@(define the-eval (make-base-eval))
@(the-eval '(require erda/sigma-rvm))

@title{@ErdaRktAssign}

@defmodulelang[erda/sigma-rvm]

The @deftech{@ErdaRktAssign} language is a variant of @ErdaRkt such that it exports an assignment expression, and modified conditionals with optional cleanup actions. Only the additions are documented here.

@ErdaRktAssign's @racket[set!] form (for variable assignment) is the same as in Racket. Bad values also get assigned.

The only conditionals available in @ErdaRktAssign are @racket[if], @racket[when], and @racket[unless]. The other conditionals from @ErdaRkt are not available. More or less all other forms (e.g., @racket[define]) and functions (e.g., @racket[raise]) from @ErdaRkt @emph{are} also included in @|ErdaRktAssign|.

@defform[(if test-expr then-expr else-expr maybe-cleanup)
#:grammar ([maybe-cleanup code:blank
                          (code:line #:cleanup cleanup-expr ...)])]{
Like @ErdaRkt-racket[if], but may include cleanup actions. Said actions are given as a sequence of @racket[cleanup-expr] expressions, which are evaluated for their side effects in the case that @racket[test-expr] yields a bad value; this does not influence the result of the overall expression, which will still be as for @ErdaRkt-racket[if].

For example:
@(interaction #:eval the-eval
  (define failed? #f)
  (if (raise 'worse) 1 2 #:cleanup (set! failed? #t))
  failed?)}

@defform[(when test-expr body ...+ maybe-cleanup)]{
Like @Racket-racket[when], but processes wrapped values, and may include cleanup actions. Where @racket[test-expr] is a good @racket[#f] value, the result of the overall expression will be @void-const, without any wrapper.

Note that there is no @racketid[when] or @racketid[unless] in @ErdaRkt, as having them makes little sense without side effects (such as assignment).

For example:
@(interaction #:eval the-eval
  (when 1
    2 3))}

@defform[(unless test-expr body ...+ maybe-cleanup)]{
Like @racket[when], but evaluates @racket[body] expressions in the case where @racket[test-expr] is a good @racket[#f] value.

For example:
@(interaction #:eval the-eval
  (unless 1
    2 3)
  (let ([x 1])
    (when (raise 'worst)
      (set! x 2)
      #:cleanup (set! x 3))
    x))}
  
@(close-eval the-eval)
