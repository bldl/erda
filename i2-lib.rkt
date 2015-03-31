#lang s-exp "i2-base.rkt"

#|

|#

(require "i2-internal.rkt"
         (only-in "util.rkt" [define* rdefine*] writeln)
         (only-in racket/base 
                  begin-for-syntax define-syntax define-syntax-rule
                  [#%app rapp] symbol?)
         (prefix-in r. racket/base)
         (only-in racket/bool symbol=?)
         (for-syntax racket/base syntax/parse))

(define-syntax define*
  (syntax-rules ()
    [(_ #:type n . more)
     (begin
       (define #:type n . more)
       (provide n))]
    [(_ (n p ...) . more)
     (begin
       (define (n p ...) . more)
       (provide n))]))

(define* #:type Result #:: (foreign))

(define* (Result? w) #:: (foreign [type (∀ T (-> (<> Result T) Bool))])
  (rapp ResultObj? w))

(declare (Good v) #:: (foreign [type (∀ T (-> T (<> Result T)))]))
(declare (Good? w) #:: (foreign [type (∀ T (-> (<> Result T) Bool))]))
(declare (Good-v w) #:: (foreign [type (∀ T (-> (<> Result T) T))]))

(declare (Bad v) #:: (foreign [type (∀ T (-> T (<> Result T)))]))
(declare (Bad? w) #:: (foreign [type (∀ T (-> (<> Result T) Bool))]))
(declare (Bad-v w) #:: (foreign [type (∀ T (-> (<> Result T) T))]))
