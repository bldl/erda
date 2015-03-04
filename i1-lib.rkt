#lang s-exp "i1-lang.rkt"

#|

The "i1-lang.rkt" language standard library.

Uses Racket's `#%app` as a Racket FFI mechanism for accessing
primitives.

|#

(require "i1-internal.rkt"
         (only-in "util.rkt" [define* rdefine*] writeln)
         (only-in racket/base 
                  begin-for-syntax define-syntax define-syntax-rule
                  [#%app rapp] symbol?)
         (prefix-in r. racket/base)
         (only-in racket/bool symbol=?)
         (for-syntax racket/base syntax/parse))

(define-syntax-rule
  (define* (n p ...) . more)
  (begin
    (define (n p ...) . more)
    (provide n)))

;; This predicate should hold for any value as long as one stays
;; within the language.
(define* (result? x) #:direct
  (rapp Good (rapp Result? x)))

(define* (good-result? x) #:direct
  (rapp Good (rapp Good? x)))

(define* (bad-result? x) #:direct
  (rapp Good (rapp Bad? x)))

(define* (result-has-value? x) #:direct
  (rapp Good (rapp Result-has-immediate-value? x)))

(define* (value-of-result x) #:handler 
  #:alert ([bad-arg pre-unless (result-has-value? x)])
  (rapp Good (rapp Result-immediate-value x)))

(define* (alert-name? x)
  (rapp Good (rapp symbol? (rapp Good-v x))))

(define* (alert-name=? x y)
  #:alert ([bad-arg pre-unless (and (alert-name? x) (alert-name? y))])
  (rapp Good (rapp symbol=? (rapp Good-v x) (rapp Good-v y))))

(define* (alert-name-of x) #:handler
  #:alert ([bad-arg pre-unless (bad-result? x)])
  (rapp Good (rapp Bad-name x)))

(define* (raise x)
  #:alert ([bad-arg pre-unless (alert-name? x)])
  (bad-condition #:original (rapp Good-v x) (r.quote-syntax raise)))

(define* (raise-with-value x v)
  #:alert ([bad-arg pre-unless (alert-name? x)])
  (bad-condition #:original (rapp Good-v x)
                 (r.quote-syntax raise-with-value)
                 #:value v))

(define* (raise-with-cause x wv) #:handler
  #:alert ([bad-arg pre-unless (and (alert-name? x) (bad-result? wv))])
  (bad-condition #:original (rapp Good-v x)
            (r.quote-syntax raise-with-cause)
            #:cause wv))

;; Where possible, recovers from `v` being a bad value by turning it
;; into a good value.
(define* (default-to-bad v) #:handler
  (if (and (bad-result? v) (result-has-value? v))
      (value-of-result v)
      v))
