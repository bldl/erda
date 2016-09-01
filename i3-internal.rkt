#lang racket/base

#|

A language implementation internal API.

|#

(require racket/contract racket/function
         racket/match racket/stxparam
         "util.rkt"
         (for-syntax racket/base racket/list racket/pretty racket/syntax
                     syntax/parse
                     "util.rkt"))

;;; 
;;; data invariants
;;; 

;; Data invariant predicate for "bare" data values. Always holds for
;; all data types that do not explicitly provide an implementation of
;; the `data-invariant?` method to indicate otherwise for some data
;; values.
(define-generics* DI
  (data-invariant? DI)
  #:defaults ([(lambda (x) #t)
               (define (data-invariant? dat) #t)]))

;;; 
;;; wrapped values
;;; 

;; Wrapped values are of type `Result`. A value `v` is of any "bare"
;; type.
(abstract-struct* Result () #:transparent)
(concrete-struct* Good Result (v) #:transparent)

(define (bare-Good val)
  (if (Good? val) (Good-v val) val))

(define (Bad-write v out mode)
  (fprintf out "(Bad ~a:" (Bad-name v))
  (define fun (Bad-fun v))
  (fprintf out " ~s" (bare-Good fun))
  (for ((arg (Bad-args v)))
    (fprintf out " ~s" (bare-Good arg)))
  (define result (Bad-result v))
  (when result
    (fprintf out " = ~s" (bare-Good result)))
  (fprintf out ")")
  (define cause (Bad-cause v))
  (when cause
    (fprintf out "⇐~s" cause)))

;; The `name` field contains any alert name symbol. The `fun` field
;; has a value of the failed operation, usually a wrapped `procedure`,
;; but can be almost anything, and also unwrapped. The `args` field
;; has the argument values of the function, as wrapped values. The
;; `result` is any invariant breaking result of the function
;; application as a wrapped value, or #f if none. The `cause` `cause`
;; is any other `Bad`ness that caused the error, or #f if none.
(struct Bad Result (name fun args result cause)
  #:transparent
  #:methods gen:custom-write
  [(define write-proc Bad-write)])
(provide (except-out (struct-out Bad) Bad))

(define-with-contract*
  (-> Bad? (or/c Result? #f) Bad?)
  (Bad-set-result bad v)
  (struct-copy Bad bad [result v]))

;; All error conditions are reported through this macro, providing as
;; much information about the nature of the error as feasible. In the
;; `#:raise` cases, (fun . args) should be such that they re-create
;; the `Bad` value.
(define-syntax* bad-condition
  (syntax-rules ()
    [(_ #:raise name fun args)
     (Bad name fun args #f #f)]
    [(_ #:raise name fun args #:value v)
     (Bad name fun args v #f)]
    [(_ #:raise name fun args #:cause cause)
     (Bad name fun args #f cause)]
    [(_ #:bad-function fun args)
     (Bad 'bad-function fun args #f #f)]
    [(_ #:bad-arg fun args)
     (Bad 'bad-arg fun args #f #f)]
    [(_ #:bad-result fun args) ;; broken DI, cannot store as `result`
     (Bad 'bad-result fun args #f #f)]
    [(_ #:precond-alert name fun args)
     (Bad name fun args #f #f)]
    [(_ #:postcond-alert name fun args result)
     (Bad name fun args result #f)]
    [(_ #:exception-alert name fun args)
     (Bad name fun args #f #f)]
    ))

;;; 
;;; result `value`
;;; 

;; An identifier that may be used to refer to a function's return
;; value in a post-condition alert expression context.
(define-syntax-parameter* value
  (syntax-rules ()))

;;; 
;;; direct operation
;;; 

(define-syntax-parameter direct-mode? #f)

(define-syntax* (begin-direct stx)
  (syntax-parse stx
    [(_ e:expr ...+)
     #'(syntax-parameterize ([direct-mode? #t])
         e ...)]))

(define-syntax* (define-my-syntax stx)
  (syntax-parse stx
    [(_ my-name:id monadic-impl:id direct-impl:id)
     #'(define-syntax (my-name ctx)
         (syntax-parse ctx
           [(_ . rest)
            (if (syntax-parameter-value #'direct-mode?)
                #'(direct-impl . rest)
                #'(monadic-impl . rest))]))]))
