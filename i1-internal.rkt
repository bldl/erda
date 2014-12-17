#lang racket/base

#|

A language implementation internal API.

|#

(require "util.rkt" racket/function racket/match)

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
;; but it must not be wrapped. A bad value `bad-v` is any unwrapped
;; value or Nothing, a `name` is an alert name symbol, `op` is an
;; operation identifier or #f, and `cause` is the causing badness (a
;; `Bad` value) or #f.
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
  (when cause
    (fprintf out "←~s" cause)))

(concrete-struct* Bad Result (bad-v name op cause) 
                  #:transparent
                  #:methods gen:custom-write
                  [(define write-proc Bad-write)])

(define* (Bad-any-value x [fail-thunk
                           (lambda () (raise-argument-error
                                  'Bad-any-value
                                  "Bad? with a value"
                                  x))])
  (let loop ((x x))
    (define v (Bad-bad-v x))
    (cond
     ((not (Nothing? v)) v)
     ((Bad-cause x) => loop)
     (else (fail-thunk)))))

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
    [(Bad (? (negate Nothing?) v) _ _ _) v]))

(define* (Bad-origin x)
  (cond
   ((Bad-cause x) => (lambda (cause) (Bad-origin cause)))
   (else x)))

(define* (Bad-origin-name x)
  (Bad-name (Bad-origin x)))
