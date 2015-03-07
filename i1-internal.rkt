#lang racket/base

#|

A language implementation internal API.

|#

(require racket/function racket/match racket/stxparam
         "util.rkt"
         (for-syntax racket/base racket/list racket/pretty
                     racket/syntax syntax/parse "util.rkt"))

;; Data invariant predicate for "bare" data values. Always holds for
;; all data types that do not explicitly provide an implementation of
;; the `data-invariant?` method to indicate otherwise for some data
;; values.
(define-generics* DI
  (data-invariant? DI)
  #:defaults ([(lambda (x) #t) (define (data-invariant? dat) #t)]))

;; Wrapped values are of type `Result`. A value `v` is of any type,
;; but it must not be wrapped.
(abstract-struct* Result () #:transparent)
(concrete-struct* Good Result (v) #:transparent)

(define (Bad-write v out mode)
  (define val (Bad-bad-v v))
  (define op (Bad-op v))
  (define cause (Bad-cause v))
  (fprintf out "~a" (Bad-name v))
  (when val
    (fprintf out "«~s»" (if (Good? val) (Good-v val) val)))
  (when op
    (fprintf out "[~a]" (syntax-e op)))
  (cond
    [(Bad-arg v) => 
     (lambda (arg)
       (fprintf out "←~s" arg))]
    [(Bad-cause v) => 
     (lambda (cause)
       (fprintf out "⇐~s" cause))]))

;; A bad value `bad-v` is any wrapped value or #f, a `name` is an
;; alert name symbol, `op` is an operation identifier or #f, `arg` is
;; any `Bad` argument (or #f) that prevented evaluation of operation
;; `op`, and `cause` is any other `Bad`ness that caused the error (or
;; #f).
(struct Bad Result (bad-v name op arg cause)
  #:transparent
  #:methods gen:custom-write
  [(define write-proc Bad-write)])
(provide (except-out (struct-out Bad) Bad))

(define* (Bad-set-bad-v bad v)
  (struct-copy Bad bad [bad-v v]))

;; All error conditions are reported through this macro, providing as
;; much information about the nature of the error as feasible.
(define-syntax* bad-condition
  (syntax-rules ()
    [(_ #:bad-arg arg op)
     (Bad #f 'bad-arg op arg #f)]
    [(_ #:data-invariant v op)
     (Bad v 'broken-DI op #f #f)]
    [(_ #:original name op)
     (Bad #f name op #f #f)]
    [(_ #:original name op #:value v)
     (Bad v name op #f #f)]
    [(_ #:original name op #:cause cause)
     (Bad #f name op #f cause)]
    [(_ #:precond-alert name op)
     (Bad #f name op #f #f)]
    [(_ #:postcond-alert name op v)
     (Bad v name op #f #f)]
    [(_ #:bad-precond cause op)
     (Bad #f 'bad-precond op #f cause)]
    [(_ #:bad-postcond cause op v)
     (Bad v 'bad-postcond op #f cause)]
    [(_ #:exception-alert name op)
     (Bad #f name op #f #f)]
    ))

;; Recursively looks for a value from the passed `Bad` value `x`,
;; traversing through any `arg` chain, returning the first available
;; (wrapped) `bad-v` value. Otherwise returns #f, or as specified by
;; `fail-thunk`.
(define (Bad-any-value x [fail-thunk (lambda () #f)])
  (let loop ([x x])
    (define v (Bad-bad-v x))
    (cond
     [v v]
     [(Bad-arg x) => loop]
     [else (fail-thunk)])))

(define* (Result-has-any-value? x)
  (cond
   ((Good? x) #t)
   ((Bad? x) (and (Bad-any-value x) #t))
   (else (raise-argument-error
          'Result-has-any-value? "Result?" x))))

(define* (Result-any-value x)
  (cond
   ((Good? x) x)
   ((and (Bad? x) (Bad-any-value x)) => (lambda (v) v))
   (else (raise-argument-error
          'Result-any-value
          "(and/c Result? Result-has-any-value?)" x))))

(define* (Result-has-immediate-value? x)
  (cond
   ((Good? x) #t)
   ((Bad? x) (and (Bad-bad-v x) #t))
   (else (raise-argument-error
          'Result-has-immediate-value? "Result?" x))))

(define* (Result-immediate-value x)
  (match x
    [(? Good?) x]
    [(? Bad? (app Bad-bad-v (? values v))) v]
    [_ (raise-argument-error
        'Result-immediate-value
        "(and/c Result? Result-has-immediate-value?)" x)]))

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
