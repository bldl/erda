#lang racket/base

#|

The language, without its reader and its standard library.

|#

(require "i3-internal.rkt"
         "util.rkt" 
         racket/bool racket/function
         racket/generic racket/list 
         racket/match racket/stxparam
         (for-syntax racket/base racket/function
                     racket/list racket/syntax
                     syntax/id-table syntax/parse
                     "util.rkt"))

(provide #%module-begin #%top #%expression
         
         ;; `require` forms
         require only-in except-in prefix-in rename-in combine-in
         relative-in only-meta-in submod
         local-require
         
         ;; `provide` forms
         provide all-defined-out all-from-out rename-out except-out
         prefix-out struct-out combine-out protect-out
         
         ;; `require` and `provide` sub-forms
         for-syntax for-template for-label for-meta

         ;; definition forms
         (rename-out [my-define define])
         declare
         
         ;; expression forms
         (rename-out [my-datum #%datum] [my-app #%app]
                     [my-quote quote] [my-if if]
                     [my-lambda lambda] [my-thunk thunk])
         begin begin0
         let let* letrec
         or and cond
         value)

;;; 
;;; literal value forms
;;; 

(define-syntax (monadic-datum stx)
  (syntax-case stx ()
    [(_ . dat)
     #'(Good (#%datum . dat))]))

(define-my-syntax my-datum monadic-datum #%datum)

(define-syntax (monadic-quote stx)
  (syntax-parse stx
    [(_ dat:id)
     #'(Good (quote dat))]))

(define-my-syntax my-quote monadic-quote quote)

(define-syntax-rule (monadic-lambda (a ...) b ...)
  (Good (lambda (a ...) b ...)))

(define-my-syntax my-lambda monadic-lambda lambda)

(define-syntax-rule (my-thunk b ...)
  (my-lambda () b ...))

;;; 
;;; function application
;;; 

;; Calls an unknown primitive, which thus has no declared error
;; unformation, but unwrapping and wrapping needs to happen in monadic
;; mode. The `proc` argument must be a `procedure?`, whereas `wargs`
;; must be wrapped arguments.
(define (monadic-apply-undeclared proc wargs)
  (cond
    [(ormap Bad? wargs)
     (bad-condition #:bad-arg (Good proc) wargs)]
    [else
     (define bargs (map Good-v wargs))
     (define bresult (apply proc bargs))
     (cond
       [(not (data-invariant? bresult))
        (bad-condition #:bad-result (Good proc) wargs)]
       [else
        (Good bresult)])]))

(define (monadic-rt-app tgt . wargs)
  (cond
    [(procedure? tgt) ;; primitive
     (monadic-apply-undeclared tgt wargs)]
    [(Good? tgt)
     (define fun (Good-v tgt))
     ;; Call the thunk that does all alert processing for a function.
     (apply fun wargs)]
    [else
     ;; Badness. Attempt to call a non-function, or a Bad function.
     ;; An implicit precondition for function application is that
     ;; the applied function must in fact be a function.
     ;; History will have the same bad function value, and thus the
     ;; same bad condition should arise by repeating the call.
     (bad-condition #:bad-function tgt wargs)]))

(define-syntax (monadic-app stx)
  (syntax-parse stx
    [(_ fun:expr args:expr ...)
     ;; Report bad `fun` as a `Bad` value.
     #'(monadic-rt-app fun args ...)]))

(define-my-syntax my-app monadic-app #%app)

;;; 
;;; direct mode
;;; 

;; Enables direct mode for a block scope. The `b ...` expressions deal
;; in bare values, except that native Erda functions work as usual.
;; The "free variables" and their values should be given as `[p e]
;; ...` for unwrapping.
(define-syntax* (anti-do stx)
  (syntax-parse stx
    [(_ ([p:id e:expr] ...) b:expr ...+)
     ;; Define a primitive for history recording.
     (define/with-syntax fun (generate-temporary 'anti-do))
     #'(letrec
           ([fun
             (lambda (p ...)
               (begin-direct b ...))])
         (monadic-apply-undeclared fun (list e ...)))]))

;;; 
;;; `if`
;;; 

;; This function is a `#:handler` in the sense that it allows a bad
;; then or else expression, long as it is not used.
(define (if-then c t-lam e-lam)
  (cond
    [(Good? c)
     (define v (Good-v c))
     (define lam
       (if v t-lam e-lam))
     (cond
       [(Good? lam) ((Good-v lam))]
       [else
        (bad-condition #:bad-arg if-then (list c t-lam e-lam))])]
    [else
     (bad-condition #:bad-arg if-then (list c t-lam e-lam))]))

(define-syntax (monadic-if stx)
  (syntax-parse stx
    [(_ c:expr t:expr e:expr)
     #'(if-then c (my-thunk t) (my-thunk e))]))

(define-my-syntax my-if monadic-if if)

;;; 
;;; error monadic sequencing
;;; 

;; Error monadic bind. Differs from monads in that `f` takes a wrapped
;; (but Good) value.
(define* >>= ;; M a -> (M b -> M b) -> M b
  (my-lambda
   (v f)
   (cond
     [(and (Good? v) (Good? f))
      ((Good-v f) v)]
     [else
      (bad-condition #:bad-arg >>= (list v f))])))

;; Syntactic sugar for `>>=`.
(define-syntax* (do stx)
  (syntax-parse stx #:datum-literals (<-)
    [(_ e:expr) 
     #'e]
    [(_ (x:id <- e:expr) rest ...+)
     #'(my-app >>= e (my-lambda (x) (do rest ...)))]
    [(_ e:expr rest ...+)
     #'(my-app >>= e (my-lambda (_) (do rest ...)))]
    ))

;;; 
;;; Racket exceptions
;;; 

;; The `name` field contains an alert name (a symbol).
(struct GotException (name))

(define-syntax-rule (make-Bad-from-exception got fun args)
  (bad-condition #:exception-alert (GotException-name got) fun args))

;;; 
;;; function definition
;;; 

(define-syntax (declare stx)
  (syntax-parse stx
    [(_ (n:id p ...) #:direct tgt:id)
     #'(define n
         (my-lambda (p ...)
           (#%app tgt p ...)))]))

(define-syntax (my-define stx)
  (syntax-parse stx
    [(_ n:id e:expr)
     #'(define n e)]
    [(_ (n:id p:id ...) #:direct b:expr ...+)
     #'(define n
         (my-lambda (p ...)
           b ...))]))
