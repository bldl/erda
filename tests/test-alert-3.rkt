#lang erda/cxx

#|

Testing an Erda primitive with both a precondition and a
postcondition.

|#

(require (only-in racket/base = -))

(define #:type Int #:: (foreign))

(declare (= x y) 
  #:: ([foreign equ] ^(-> Int Int Bool)))

(declare (- x y) 
  #:: ([foreign sub] ^(-> Int Int Int))
  #:alert ([zero-arg pre-when (or (= x 0) (= y 0))]
           [zero-res post-when (= value 0)]))

(define (main x y) #:: (export)
  (- x y))

(main 2 1)
(main 1 1)
(main 0 1)
(main 1 0)

