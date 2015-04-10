#lang erda/cxx

#|

Testing an Erda primitive.

Although `+` implementation does not deal in wrapped values, due to
auto-wrapping callers still see it as if it did. Still, we must
declare each function is it actually is.

|#

(require (only-in racket/base +))

(define #:type Int #:: (foreign))

(declare (+ x y) 
  #:: ([foreign add] ^(-> Int Int Int)))

(define (main x y) #:: (export)
  (+ x y))

(main 4 5)
