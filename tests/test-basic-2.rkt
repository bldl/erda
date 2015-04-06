#lang erda/cxx

#|
|#

(define #:type Symbol #:: (foreign))

(define (identity x)
  x)

(define (main) #:: (export ^(-> (<> Result Symbol)))
  (identity 'top-level-expression))

(main)
