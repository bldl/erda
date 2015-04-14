#lang erda/cxx

#|

|#

(require (only-in racket/base +))

(define #:type Int #:: ([foreign int]))

(declare (+ x y)
  #:: ([foreign add] ^(-> Int Int Int)))

(define (run x y) #:: (export)
  (+ x y))
