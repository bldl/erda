#lang erda/cxx

#|

Integer operations that detect overflow. The arithmetic Racket
operations that are used for simulation do not overflow at all.

|#

(require (only-in racket/base = < > + - *))

(provide Int = < > + - *)

(define #:type Int #:: ([foreign int]))

(declare (= x y)
  #:: ([foreign equ] ^(-> Int Int Bool)))

(declare (< x y)
  #:: ([foreign lt] ^(-> Int Int Bool)))

(declare (> x y)
  #:: ([foreign gt] ^(-> Int Int Bool)))

(declare (+ x y) #:direct
  #:: ([foreign add] ^(->Result Int Int Int)))

(declare (- x y) #:direct
  #:: ([foreign sub] ^(->Result Int Int Int)))

(declare (* x y) #:direct
  #:: ([foreign mul] ^(->Result Int Int Int)))
