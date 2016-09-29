#lang s-exp "i3-surface.rkt"

#|

The language standard library.

|#

(require "i3-internal.rkt"
         (only-in "util.rkt" [define* rdefine*])
         (only-in racket/base [list rlist] map andmap ormap
                  [if rif] [or ror] [and rand]
                  [#%datum rdatum]
                  begin-for-syntax define-syntax define-syntax-rule
                  quote-syntax symbol? [apply rapply] [#%app rapp])
         (only-in racket/bool symbol=?)
         (for-syntax racket/base racket/syntax syntax/parse))

(define-syntax-rule
  (define* (n . p) . more)
  (begin
    (define (n . p) . more)
    (provide n)))

(define-syntax-rule
  (declare* (n . p) . more)
  (begin
    (declare (n . p) . more)
    (provide n)))

;;; 
;;; wrapped-value inspection
;;;

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
  (rapp Bad-fun x))

(define* (bad-result-args x) #:handler
  #:alert ([bad-arg pre-unless (bad-result? x)])
  (rapp Good (rapp Bad-args x)))

(define* (result-value x) #:handler 
  #:alert ([bad-arg pre-unless (result-has-value? x)])
  (rapp Result-immediate-value x))

;;; 
;;; recovery
;;; 

;; Where possible, recovers from `v` being a bad value by turning it
;; into a good value.
(define* (default-to-bad v) #:handler
  (if (and (bad-result? v) (result-has-value? v))
      (result-value v)
      v))

;;; 
;;; "throwing"
;;; 

(define* (raise x)
  #:alert ([bad-arg pre-unless (alert-name? x)])
  (bad-condition #:raise (rapp Good-v x) raise (rapp rlist x)))

(define* (raise-with-value x v)
  #:alert ([bad-arg pre-unless (alert-name? x)])
  (bad-condition #:raise (rapp Good-v x)
                 raise-with-value (rapp rlist x v)
                 #:value v))

(define* (raise-with-cause x cause) #:handler
  #:alert ([bad-arg pre-unless (and (alert-name? x) (bad-result? cause))])
  (bad-condition #:raise (rapp Good-v x)
                 raise-with-cause (rapp rlist x cause)
                 #:cause cause))

;;; 
;;; `not`
;;; 

(require
 (prefix-in rkt. (only-in racket/base not)))

(declare* (not x) #:is rkt.not)

;;; 
;;; lists and pairs
;;;

(require
 (prefix-in rkt. (only-in racket/base
                          null null?
                          list?
                          length
                          cons pair? car cdr)))
                          
(rdefine* null
  (rapp Good rkt.null))

(declare* (null? x) #:is rkt.null?)

(declare* (list . args) #:is rlist)

(declare* (list? x) #:is rkt.list?)

(declare* (length lst) #:is rkt.length
  #:alert ([bad-arg pre-unless (list? lst)]))

(declare* (cons x y) #:is rkt.cons)

(declare* (pair? x) #:is rkt.pair?)

(declare* (car p) #:is rkt.car
  #:alert ([bad-arg pre-unless (pair? p)]))

(declare* (cdr p) #:is rkt.cdr
  #:alert ([bad-arg pre-unless (pair? p)]))

;;; 
;;; arithmetic
;;; 

(require
 (prefix-in rkt. (only-in racket/base real? >= <)))

(declare (real? x) #:is rkt.real?)

(declare (< x y) #:is rkt.<
  #:alert ([bad-arg pre-unless (and (real? x) (real? y))]))

(declare (>= x y) #:is rkt.>=
  #:alert ([bad-arg pre-unless (and (real? x) (real? y))]))

;;; 
;;; argument list manipulation
;;; 

(require
 (prefix-in rkt. (only-in racket/list list-set)))

(define* (args-list? x)
  (let ((x (rapp Good-v x)))
    (rif (rand (rapp rkt.list? x) (rapp andmap Result? x)) #t #f)))

(define* (args-list . args) #:handler
  #:alert ([bad-arg pre-unless (rapp Good
                                     (rapp andmap Result?
                                           (rapp Good-v args)))])
  args)

(define* (args-cons x args) #:handler
  #:alert ([bad-arg pre-unless (and (result? x) (list? args))])
  (rapp Good (rapp rkt.cons x (rapp Good-v args))))

(define* (args-car args)
  (rapp rkt.car (rapp Good-v args)))

(define* (args-cdr args)
  #:alert ([bad-arg pre-unless (pair? args)])
  (rapp Good (rapp rkt.cdr (rapp Good-v args))))

(define* (args-replace-first args v) #:handler
  (args-cons v (args-cdr args)))

(define* (args-list-set args pos v) #:handler
  #:alert ([bad-arg pre-unless (and (pair? args)
                                    (>= pos 0)
                                    (< pos (length args))
                                    (result? v))])
  (rapp Good (rapp rkt.list-set
                   (rapp Good-v args)
                   (rapp Good-v pos)
                   v)))

;;; 
;;; redoing
;;; 

(define* (redo v) #:handler
  #:alert ([bad-arg pre-unless (bad-result? v)])
  (apply (bad-result-fun v) (bad-result-args v)))

(define* (redo-apply v args) #:handler
  #:alert ([bad-arg pre-unless (and (bad-result? v) (args-list? args))])
  (apply (bad-result-fun v) args))

(define* (redo-app v . args) #:handler
  #:alert ([bad-arg pre-unless (and (bad-result? v) (args-list? args))])
  (apply (bad-result-fun v) args))

;;; 
;;; error monadic sequencing
;;; 

(require (only-in racket/base procedure? procedure-arity-includes?))
(provide function-with-arity?)

(define-direct (function-with-arity? f arity)
  (and (procedure? f)
       (procedure-arity-includes? f arity)))

(provide do >>=)

;; Identity monadic bind, which due to the language is error monadic,
;; too. Its type signature M a -> (M b -> M b) -> M b differs from
;; `Monad`s in that `f` takes a wrapped (but `Good`) value, but in a
;; more abstract sense it is still like an identity monad's bind
;; operation.
(define (>>= v f)
  #:alert ([bad-arg pre-unless (function-with-arity? f 1)]) 
  (f v))
;; The implementation is as for the identity monad, due to our
;; language semantics, even though `M` is not an identity function on
;; types, meaning that the values are indeed wrapped.

;; Syntactic sugar for nested invocations of `>>=`.
(define-syntax (do stx)
  (syntax-parse stx #:datum-literals (<-)
    [(_ e:expr) 
     #'e]
    [(_ [x:id <- e:expr] rest ...+)
     (define/with-syntax lam
       (syntax/loc stx (lambda (x) (do rest ...))))
     #'(e . >>= . lam)]
    [(_ e:expr rest ...+)
     (define/with-syntax lam
       (syntax/loc stx (lambda (_) (do rest ...))))
     #'(e . >>= . lam)]
    ))
