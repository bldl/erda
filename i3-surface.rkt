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
;;         (rename-out [my-define define])
  ;;       declare
         
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
;;; Racket exceptions
;;; 

;; The `name` field contains an alert name (a symbol).
(struct GotException (name))

(define-syntax-rule (make-Bad-from-exception got fun args)
  (bad-condition #:exception-alert (GotException-name got) fun args))

;;; 
;;; function application
;;; 

(define (monadic-rt-app wfun . wargs)
  (cond
    [(Bad? wfun)
     ;; Badness. Attempt to call a non-function, or a Bad function.
     ;; History will have the same bad function value, and thus the
     ;; same bad condition should arise by repeating the call.
     (bad-condition #:bad-function wfun wargs)]
    [else
     (define fun (Good-v wfun))
     (cond
       [(not (procedure? fun))
        ;; An implicit precondition for function application is that
        ;; the applied function must in fact be a function.
        (bad-condition #:bad-function wfun wargs)]
       [else
        ;; Call the thunk that does all alert processing for a function.
        (apply fun wargs)])]))

(define-syntax (monadic-app stx)
  (syntax-parse stx
    [(_ fun:expr args:expr ...)
     ;; Report bad `fun` as a `Bad` value.
     #'(monadic-rt-app fun args ...)]))

(define-my-syntax my-app monadic-app #%app)

;;; 
;;; `if`
;;; 

(define-syntax (monadic-if stx)
  (syntax-parse stx
    [(_ c:expr t:expr e:expr)
     #'(if-then c (my-thunk t) (my-thunk e))]))

(define-my-syntax my-if monadic-if if)
