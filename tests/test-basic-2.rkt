#lang erda/cxx

#|

Testing an Erda function.

|#

(define #:type Symbol #:: (foreign))

(define (identity x)
  x)

(define (main) #:: (export ^(-> (<> Result Symbol)))
  (identity 'top-level-expression))

(main)
