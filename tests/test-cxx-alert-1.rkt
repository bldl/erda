#lang erda/cxx

#|

Testing an Erda primitive with a precondition.

|#

(require (only-in racket/base = /))

(define #:type Int #:: (foreign))

(declare (= x y) 
  #:: ([foreign equ] ^(-> Int Int Bool)))

(declare (/ x y) 
  #:: ([foreign div] ^(-> Int Int Int))
  #:alert ([div-by-0 pre-when (= y 0)]))

(define (main x y) #:: (export)
  (/ x y))

(main 4 5)
(main 6 0)
