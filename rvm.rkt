#lang racket/base

#|

About this language:
- Racket module system is supported (imports and exports)
- Racket macros or compile-time code is not supported
- (quasi)quoting of data is not supported in general (symbols are)
- only first-class function application is supported
- only single static assignment is supported (no `set!`)
- all expressions evaluate to a single value (no `values`)
- accordingly, there are no multi-variable binding forms
- there is no unit type (and no `void`, `when`, or `unless`)

For R&D support, we do allow Racket to be used:
- `require` may be used to bring in Racket names globally
- `local-require` may be used to do the same locally
- `let-racket-*` forms are convenient for giving Racket expressions
- `bare` may be used to give unwrapped Racket literals
With great power comes great responsibility not to break assumptions.

|#

(module reader syntax/module-reader 
  erda/rvm)

(require "i1-surface.rkt" "i1-lib.rkt")
(provide (all-from-out "i1-surface.rkt" "i1-lib.rkt"))

(provide #%top-interaction)
