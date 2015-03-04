#lang racket/base

#|

A language implementation internal API.

|#

(require racket/function racket/match
         "util.rkt"
         (for-syntax racket/base syntax/parse))

;; Data invariant predicate for "bare" data values. Always holds for
;; all data types that do not explicitly provide an implementation of
;; the `data-invariant?` method to indicate otherwise for some data
;; values.
(define-generics* DI
  (data-invariant? DI)
  #:defaults ([(lambda (x) #t) (define (data-invariant? dat) #t)]))

;; As #f is also a valid "bare" value, we use `Nothing` to indicate
;; non-existence of a value.
(singleton-struct* Nothing (the-Nothing) () #:transparent)

;; Wrapped values are of type `Result`. A value `v` if of any type,
;; but it must not be wrapped.
(abstract-struct* Result () #:transparent)
(concrete-struct* Good Result (v) #:transparent)

(define (Bad-write v out mode)
  (define val (Bad-bad-v v))
  (define op (Bad-op v))
  (define cause (Bad-cause v))
  (fprintf out "~a" (Bad-name v))
  (unless (Nothing? val)
    (fprintf out "«~s»" val))
  (when op
    (fprintf out "[~a]" (syntax-e op)))
  (cond
    [(Bad-arg v) => 
     (lambda (arg)
       (fprintf out "←~s" arg))]
    [(Bad-cause v) => 
     (lambda (cause)
       (fprintf out "⇐~s" cause))]))

;; A bad value `bad-v` is any unwrapped value or Nothing, a `name` is
;; an alert name symbol, `op` is an operation identifier or #f, `arg`
;; is any `Bad` argument (or #f) that prevented evaluation of
;; operation `op`, and `cause` is any other `Bad`ness that caused the
;; error (or #f).
(struct Bad Result (bad-v name op arg cause)
  #:transparent
  #:methods gen:custom-write
  [(define write-proc Bad-write)])
(provide (except-out (struct-out Bad) Bad))

(define-syntax-rule (make-Bad-or-handle bad-v name op arg cause)
  (Bad bad-v name op arg cause))

;; All error conditions are reported through this macro, providing as
;; much information about the nature of the error as feasible.
(define-syntax* bad-condition
  (syntax-rules ()
    [(_ #:bad-arg arg op)
     (make-Bad-or-handle the-Nothing 'bad-arg op arg #f)]
    [(_ #:data-invariant bare-v op) ;; FIXME should we take wrapped values?
     (make-Bad-or-handle bare-v 'broken-DI op #f #f)]
    [(_ #:original name op)
     (make-Bad-or-handle the-Nothing name op #f #f)]
    [(_ #:original name op #:value v)
     (make-Bad-or-handle (Good-v v) name op #f #f)]
    [(_ #:original name op #:cause cause)
     (make-Bad-or-handle the-Nothing name op #f cause)]
    [(_ #:precond-alert name op)
     (make-Bad-or-handle the-Nothing name op #f #f)]
    [(_ #:postcond-alert name op v) ;; FIXME a handler's precondition could also fail, on a bad result, and thus we should probably be storing wrapped values (or have them be #f)
     (make-Bad-or-handle (Good-v v) name op #f #f)]
    [(_ #:bad-precond cause op)
     (make-Bad-or-handle the-Nothing 'bad-precond op #f cause)]
    [(_ #:bad-postcond cause op v) ;; FIXME a handler's precondition could also fail, on a bad result, and thus we should probably be storing wrapped values (or have them be #f)
     (make-Bad-or-handle (Good-v v) 'bad-postcond op #f cause)]
    [(_ #:exception-alert name op)
     (make-Bad-or-handle the-Nothing name op #f #f)]
    ))

;; Recursively looks for a value from the passed `Bad` value `x`,
;; traversing through any `arg` chain to find the bad value of the
;; innermost subexpression that failed or referenced a bad variable.
;; Calls `fail-thunk` if no value is found.
(define (Bad-any-value x [fail-thunk
                          (lambda () (raise-argument-error
                                 'Bad-any-value
                                 "Bad? with a value"
                                 x))])
  (let loop ([x x])
    (define v (Bad-bad-v x))
    (cond
     [(not (Nothing? v)) v]
     [(Bad-arg x) => loop]
     [else (fail-thunk)])))

(define* (Result-has-any-value? x)
  (cond
   ((Good? x) #t)
   ((Bad? x) (not (Nothing? (Bad-any-value x (lambda () the-Nothing)))))
   (else #f)))

(define* (Result-any-value x)
  (cond-or-fail
   ((Good? x) (Good-v x))
   ((Bad? x) (Bad-any-value x))))

(define* (Result-has-immediate-value? x)
  (cond
   ((Good? x) #t)
   ((Bad? x) (not (Nothing? (Bad-bad-v x))))
   (else #f)))

(define* (Result-immediate-value x)
  (match x
    [(Good v) v]
    [(? Bad? (app Bad-bad-v (? (negate Nothing?) v))) v]
    ))

(define (Bad-root-cause x)
  (let loop ((x x))
    (cond
      ((Bad-arg x) => loop)
      ((Bad-cause x) => loop)
      (else x))))

(define (Bad-origin x)
  (cond
   ((Bad-arg x) => (lambda (arg) (Bad-origin arg)))
   (else x)))

;; Our `try` construct matches based on this name.
(define* (Bad-origin-name x)
  (Bad-name (Bad-origin x)))
