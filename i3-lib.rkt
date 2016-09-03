#lang s-exp "i3-surface.rkt"

#|

The language standard library.

|#

(require "i3-internal.rkt"
         (only-in "util.rkt" [define* rdefine*])
         (only-in racket/base 
                  begin-for-syntax define-syntax define-syntax-rule
                  quote-syntax symbol? list
                  [#%app rapp])
         (only-in racket/bool symbol=?)
         (for-syntax racket/base syntax/parse))

(define-syntax-rule
  (define* (n p ...) . more)
  (begin
    (define (n p ...) . more)
    (provide n)))

(define* (result? x) #:direct
  (rapp Good (rapp Result? x)))

(define* (good-result? x) #:direct
  (rapp Good (rapp Good? x)))

(define* (bad-result? x) #:direct
  (rapp Good (rapp Bad? x)))

(define* (result-has-value? x) #:direct
  (rapp Good (rapp Result-has-immediate-value? x)))

(define* (alert-name? x)
  (rapp Good (rapp symbol? (rapp Good-v x))))

(define* (alert-name=? x y)
  #:alert ([bad-arg pre-unless (and (alert-name? x) (alert-name? y))])
  (rapp Good (rapp symbol=? (rapp Good-v x) (rapp Good-v y))))

(define* (bad-result-alert-name x) #:handler
  #:alert ([bad-arg pre-unless (bad-result? x)])
  (rapp Good (rapp Bad-name x)))

(define* (bad-result-fun x) #:handler
  #:alert ([bad-arg pre-unless (bad-result? x)])
  (rapp Good (rapp Bad-fun x)))

(define* (bad-result-args x) #:handler
  #:alert ([bad-arg pre-unless (bad-result? x)])
  (rapp Good (rapp Bad-args x)))

(define* (result-value x) #:handler 
  #:alert ([bad-arg pre-unless (result-has-value? x)])
  (rapp Result-immediate-value x))

;; Where possible, recovers from `v` being a bad value by turning it
;; into a good value.
(define* (default-to-bad v) #:handler
  (if (and (bad-result? v) (result-has-value? v))
      (result-value v)
      v))

(define* (raise x)
  #:alert ([bad-arg pre-unless (alert-name? x)])
  (bad-condition #:raise (rapp Good-v x) raise (rapp list x)))

(define* (raise-with-value x v)
  #:alert ([bad-arg pre-unless (alert-name? x)])
  (bad-condition #:raise (rapp Good-v x)
                 raise-with-value (rapp list x v)
                 #:value v))

(define* (raise-with-cause x cause) #:handler
  #:alert ([bad-arg pre-unless (and (alert-name? x) (bad-result? cause))])
  (bad-condition #:raise (rapp Good-v x)
                 raise-with-cause (rapp list x cause)
                 #:cause cause))
