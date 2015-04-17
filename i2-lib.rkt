#lang s-exp "i2-base.rkt"

#|

|#

(require "i2-internal.rkt"
         (only-in "util.rkt" [define* rdefine*] writeln)
         (only-in racket/base 
                  begin-for-syntax define-syntax define-syntax-rule
                  [#%app rapp] not symbol?)
         (prefix-in r. racket/base)
         (only-in racket/bool symbol=?)
         (for-syntax racket/base syntax/parse))

(define-syntax declare*
  (syntax-rules ()
    [(_ (n p ...) . more)
     (begin
       (declare (n p ...) . more)
       (provide n))]))

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
;;; Magnolisp types
;;; 

(declare #:type Bool #:: ([foreign bool]))
(declare #:type Void #:: ([foreign void]))

;;; 
;;; Erda types
;;; 

(define* #:type AlertName
  #:: (foreign [literal (display type-id) "(" (cxx-str datum) ")"]))

;;; 
;;; Racket functions
;;; 

(declare* (not x) 
  #:: ([foreign !] [type (-> Bool Bool)]))

;;; 
;;; data invariant
;;; 

(declare (data-invariant? v) #:direct
  #:: (foreign [type (∀ T (-> T Bool))]))

;;; 
;;; optional type
;;;

(define* #:type Maybe #:: (foreign))

(declare (Just v) #:direct
  #:: ([foreign Just] [type (∀ T (-> T (<> Maybe T)))]))
(declare (Just? w) #:direct
  #:: (foreign [type (∀ T (-> (<> Maybe T) Bool))]))
(declare (Just-v w) #:direct
  #:: (foreign [type (∀ T (-> (<> Maybe T) T))]))

(declare (Nothing) #:direct
  #:: ([foreign Nothing] [type (∀ T (-> (<> Maybe T)))]))
(declare (Nothing? w) #:direct
  #:: (foreign [type (∀ T (-> (<> Maybe T) Bool))]))

;;; 
;;; wrapper type
;;;

(define* #:type Result #:: (foreign))

(declare (Good v) #:direct
  #:: ([foreign Good] [type (∀ T (-> T (<> Result T)))]))
(declare (Good? w) #:direct
  #:: (foreign [type (∀ T (-> (<> Result T) Bool))]))
(declare (Good-v w) #:direct
  #:: (foreign [type (∀ T (-> (<> Result T) T))]))

(declare (Bad v name) #:direct
  #:: ([foreign Bad] 
       [type (∀ T (-> (<> Maybe (<> Result T)) AlertName (<> Result T)))]))
(declare (Bad? w) #:direct
  #:: (foreign [type (∀ T (-> (<> Result T) Bool))]))
(declare (Bad-v w) #:direct
  #:: (foreign [type (∀ T (-> (<> Result T) (<> Maybe (<> Result T))))]))
(declare (Bad-name w) #:direct
  #:: (foreign [type (∀ T (-> (<> Result T) AlertName))]))
