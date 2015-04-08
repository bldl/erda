#lang erda/cxx

#|

Testing an Erda literal.

|#

(define #:type Symbol #:: (foreign))

(define (main) #:: (export ^(-> (<> Result Symbol)))
  'top-level-expression)

(main)
