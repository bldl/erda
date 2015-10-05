#lang erda/cxx

#|

Integer operations that detect overflow.

|#

(require (only-in racket/base = + - *))

(define #:type Int #:: ([foreign int]))

(declare (= x y)
  #:: ([foreign equ] ^(-> Int Int Bool)))

(declare (+ x y) #:direct
  #:: ([foreign add] ^(-> (<> Result Int) (<> Result Int) (<> Result Int))))

(declare (- x y) #:direct
  #:: ([foreign sub] ^(-> (<> Result Int) (<> Result Int) (<> Result Int))))

(declare (* x y) #:direct
  #:: ([foreign mul] ^(-> (<> Result Int) (<> Result Int) (<> Result Int))))

(define (twice x) #:: (export)
  (* 2 x))

(define (squared x) #:: (export)
  (* x x))

(define (pow3 x) #:: (export)
  (* (* x x) x))

(define (add-is-mul x y) #:: (export)
  (= (+ x y) (* x y)))

(define (add-is-sub x y) #:: (export)
  (= (+ x y) (- x y)))

(define (add-is-sub-is-mul x y) #:: (export)
  (and (add-is-mul x y) (add-is-sub x y)))
