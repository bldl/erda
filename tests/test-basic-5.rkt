#lang erda/cxx

#|

Testing an Erda primitive with a made-up postcondition.

|#

(require (only-in racket/base = -))

(define #:type Int #:: (foreign))

(declare (= x y) 
  #:: ([foreign equ] ^(-> Int Int Bool)))

(declare (- x y) 
  #:: ([foreign sub] ^(-> Int Int Int))
  #:alert ([do-not-like-0 post-when (= value 0)]))

(define (main x y) #:: (export)
  (- x y))

(main 4 4)
(main 6 0)
