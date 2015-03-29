#lang s-exp "i2-base.rkt"

#|

|#

(require "i2-internal.rkt"
         (only-in "util.rkt" [define* rdefine*] writeln)
         (only-in racket/base 
                  begin-for-syntax define-syntax define-syntax-rule
                  [#%app rapp] symbol?)
         (prefix-in r. racket/base)
         (only-in racket/bool symbol=?)
         (for-syntax racket/base syntax/parse))
