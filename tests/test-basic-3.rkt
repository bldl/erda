#lang erda/cxx

#|

Testing an Erda primitive with a precondition.

Although `+` implementation does not deal in wrapped values, due to
auto-wrapping callers still see it as having a signature with wrapped
types.

|#

(require (only-in racket/base [+ r.+]))

(define #:type Int #:: (foreign))

(define (+ x y) #:: 
  ([foreign add]
   ^(-> (<> Result Int) (<> Result Int) (<> Result Int)))
  (r.+ x y))

(define (main x y) #:: (export)
  (+ x y))

(main 4 5)
