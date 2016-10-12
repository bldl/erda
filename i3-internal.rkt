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

(define* print-Bad-concisely?
  (make-parameter #f))

(define (concise-procedure-name? s)
  (and (regexp-match-exact? #px"[[:graph:]]+" s)
       (not (regexp-match? #px"[/:]" s))))

(define (concise-procedure-name v)
  (define n (object-name v))
  (cond
    [(symbol? n)
     (define s (symbol->string n))
     (cond
       [(concise-procedure-name? s)
        n]
       [else
        '<fun>])]
    [else '<fun>]))

(define (bare-Good val)
  (cond
    [(Good? val)
     (define v (Good-v val))
     (cond
       [(and (procedure? v)
             (print-Bad-concisely?))
        (concise-procedure-name v)]
       [else
        v])]
    [else val]))

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
;; has a value of the failed operation, as a wrapped `procedure`. The
;; `args` field has the argument values of the function, as wrapped
;; values. The `result` is any invariant breaking result of the
;; function application as a wrapped value, or #f if none. The `cause`
;; `cause` is any other `Bad`ness that caused the error, or #f if
;; none.
(struct Bad Result (name fun args result cause)
  #:transparent
  #:methods gen:custom-write
  [(define write-proc Bad-write)])
(provide (struct-out Bad))

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

(define-with-contract*
  (-> Bad? (or/c Result? #f) Bad?)
  (Bad-set-result bad v)
  (struct-copy Bad bad [result v]))

(define* (Result-has-immediate-value? x)
  (cond
   [(Good? x) #t]
   [(Bad? x) (and (Bad-result x) #t)]
   [else (raise-argument-error
          'Result-has-immediate-value? "Result?" x)]))

(define* (Result-immediate-value x)
  (match x
    [(? Good?) x]
    [(? Bad? (app Bad-result (? values v))) v]
    [_ (raise-argument-error
        'Result-immediate-value
        "(and/c Result? Result-has-immediate-value?)" x)]))

;; Our `try` construct matches by searching for an alert name
;; recursively.
(define-with-contract*
  (-> Result? symbol? boolean?)
  (Result-contains-name? v n)
  (define (f? v)
    (and (Bad? v)
         (or
          (eq? n (Bad-name v))
          (f? (Bad-fun v))
          (ormap f? (Bad-args v)))))
  (f? v))

;;; 
;;; contracts
;;;

;; Only meant to be used for documentation. In particular, not to be
;; invoked with Erda semantics.
(define* ((Result/c f?) x)
  (cond
    [(Good? x) (f? (Good-v x))]
    [(Bad? x) #t]
    [else #f]))

;; Only meant to be used for documentation. In particular, not to be
;; invoked with Erda semantics.
(define* ((Good/c f?) x)
  (cond
    [(Good? x) (f? (Good-v x))]
    [else #f]))

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
