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

;;; 
;;; optional type
;;;

(define* #:type Maybe #:: (foreign))

(define* (Maybe? w) 
  #:: (foreign [type (∀ T (-> (<> Maybe T) Bool))])
  (rapp MaybeObj? w))

(declare (Just v) 
  #:: ([foreign Just] [type (∀ T (-> T (<> Maybe T)))]))
(declare (Just? w) 
  #:: (foreign [type (∀ T (-> (<> Maybe T) Bool))]))
(declare (Just-v w) 
  #:: (foreign [type (∀ T (-> (<> Maybe T) T))]))

(declare (Nothing)
  #:: ([foreign Nothing] [type (∀ T (-> (<> Maybe T)))]))
(declare (Nothing? w) 
  #:: (foreign [type (∀ T (-> (<> Maybe T) Bool))]))

;;; 
;;; wrapper type
;;;

(define* #:type Result #:: (foreign))

(define* (Result? w) 
  #:: (foreign [type (∀ T (-> (<> Result T) Bool))])
  (rapp ResultObj? w))

(declare (Good v) 
  #:: ([foreign Good] [type (∀ T (-> T (<> Result T)))]))
(declare (Good? w) 
  #:: (foreign [type (∀ T (-> (<> Result T) Bool))]))
(declare (Good-v w) 
  #:: (foreign [type (∀ T (-> (<> Result T) T))]))

(declare (Bad v) 
  #:: ([foreign Bad] [type (∀ T (-> (<> Maybe T) (<> Result T)))]))
(declare (Bad? w) 
  #:: (foreign [type (∀ T (-> (<> Result T) Bool))]))
(declare (Bad-v w) 
  #:: (foreign [type (∀ T (-> (<> Result T) (<> Maybe T)))]))
