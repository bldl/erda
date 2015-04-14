#lang erda/cxx

#|

Pre- and post-conditions that do not like zero values.

|#

(require (only-in racket/base = -))

(define #:type Int #:: ([foreign int]))

(declare (= x y)
  #:: ([foreign equ] ^(-> Int Int Bool)))

(declare (- x y)
  #:: ([foreign sub] ^(-> Int Int Int))
  #:alert ([zero-arg pre-when (or (= x 0) (= y 0))]
           [zero-res post-when (= value 0)]))

(define (run x y) #:: (export)
  (- x y))
