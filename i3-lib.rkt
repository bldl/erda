#lang s-exp "i3-surface.rkt"

#|

The language standard library.

|#

(require "i3-internal.rkt"
         (only-in "util.rkt" [define* rdefine*])
         (only-in racket/base 
                  begin-for-syntax define-syntax define-syntax-rule
                  quote-syntax symbol?
                  [#%app rapp])
         (only-in racket/bool symbol=?)
         (for-syntax racket/base syntax/parse))

(define-syntax-rule
  (define* (n p ...) . more)
  (begin
    (define (n p ...) . more)
    (provide n)))

(define* (result? x) #:direct
  (rapp Good (rapp Result? x)))

(define* (good-result? x) #:direct
  (rapp Good (rapp Good? x)))

(define* (bad-result? x) #:direct
  (rapp Good (rapp Bad? x)))

(define* (result-has-value? x) #:direct
  (rapp Good (rapp Result-has-immediate-value? x)))
