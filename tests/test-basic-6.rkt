#lang erda/cxx

#|

Testing the `not` primitive function, which obeys a foreign calling
convention, for which adaptation should be automatic.

|#

(require (only-in racket/base))

(define (main x) #:: (export)
  (not x))

(main #t)
(main #f)

