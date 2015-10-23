#lang erda/cxx

#|

Factorial with integer overflow detection.

|#

(require "arith.rkt")

(define (factorial x) #:: (export ^(->Result Int Int))
  #:alert ([bad-arg pre-when (< x 0)])
  (cond
   [(= x 0) 1]
   [else (* x (factorial (- x 1)))]))
