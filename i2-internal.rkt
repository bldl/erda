#lang racket/base

#|

A language implementation internal API. The symbols exported here are
generally not a part of the `erda/cxx` language.

|#

(require racket/function racket/match racket/stxparam
         magnolisp/core magnolisp/modbeg
         "util.rkt"
         (for-syntax 
          racket/base racket/list racket/pretty
          racket/syntax syntax/parse "util.rkt"))

;;; 
;;; wrapper data structure
;;; 

(define* (data-invariant? x) #t)

(abstract-struct* MaybeObj () #:transparent)
(concrete-struct* Nothing MaybeObj () #:transparent)
(concrete-struct* Just MaybeObj (v) #:transparent)

(abstract-struct* ResultObj () #:transparent)

;; Field `v` :: T.
(concrete-struct* Good ResultObj (v) #:transparent)

(define (Bad-write bad out mode)
  (fprintf out "(Bad")
  (fprintf out " ~a" (Bad-name bad))
  (let ((val (Bad-v bad)))
    (unless (Nothing? val)
      (fprintf out "«~s»" (Just-v val))))
  (fprintf out ")"))

;; The `v` :: Maybe<Result<T>> field contains an optional wrapped
;; value that did not satisfy an invariant. The `name` field contains
;; an alert name symbol.
(concrete-struct* Bad ResultObj (v name)
  #:methods gen:custom-write [(define write-proc Bad-write)]
  #:transparent)

;; All error conditions are reported through this macro. This means
;; that `Bad` values are not created based on their content, but
;; rather then error situation.
(define-syntax* bad-condition
  (syntax-rules ()
    [(_ #:bad-arg arg op)
     (Bad (Nothing) 'bad-arg)]
    [(_ #:data-invariant v op args)
     (Bad (Just v) 'data-invariant)]
    [(_ #:original name op)
     (Bad (Nothing) name)]
    [(_ #:original name op #:value v)
     (Bad (Just v) name)]
    [(_ #:original name op #:cause cause)
     (Bad (Nothing) name)]
    [(_ #:precond-alert name op args)
     (Bad (Nothing) name)]
    [(_ #:postcond-alert name op v args)
     (Bad (Just v) name)]
    [(_ #:bad-precond cause op args)
     (Bad (Nothing) 'bad-precond)]
    [(_ #:bad-postcond cause op v args)
     (Bad (Just v) 'bad-postcond)]
    ))

(define-syntax* let-Good-args
  (syntax-rules ()
    [(_ () #:op op-id #:then then ...)
     (let () then ...)]
    [(_ ([p e] . rest) #:op op-id #:then then ...)
     (let ([p e])
       (if (Bad? p)
           (bad-condition #:bad-arg p #'op-id)
           (let-Good-args rest #:op op-id #:then then ...)))]))

;;; 
;;; alert condition checks
;;;

(define-syntax* cond-pre-checks
  (syntax-rules ()
    [(_ #:op op args #:checks () #:then then ...)
     (let () then ...)]
    [(_ #:op op args #:checks ([name c-e] . rest) #:then then ...)
     (let ([c-v c-e])
       (cond
         [(Bad? c-v) 
          (bad-condition #:bad-precond c-v #'op args)]
         [(Good-v c-v) 
          (bad-condition #:precond-alert name #'op args)]
         [else 
          (cond-pre-checks #:op op args #:checks rest #:then then ...)]))]))

(define-syntax sub-cond-post-checks
  (syntax-rules ()
    [(_ #:op op args #:checks () #:then r)
     r]
    [(_ #:op op args #:checks ([name c-e] . rest) #:then r)
     (let ([c-v c-e])
       (cond
         [(Bad? c-v) 
          (bad-condition #:bad-postcond c-v #'op r args)]
         [(Good-v c-v) 
          (bad-condition #:postcond-alert name #'op r args)]
         [else 
          (sub-cond-post-checks #:op op args #:checks rest #:then r)]))]))
    
(define-syntax* cond-post-checks
  (syntax-rules ()
    [(_ #:op op args #:checks () #:then r)
     r]
    [(_ #:op op args #:checks checks #:then r)
     (syntax-parameterize ([value
                            (make-rename-transformer #'r)])
       (sub-cond-post-checks #:op op args #:checks checks #:then r))]))
    
;;; 
;;; result `value`
;;; 

;; An identifier that may be used to refer to a function's return
;; value in a post-condition alert expression context.
(define-syntax-parameter* value
  (syntax-rules ()))

;;; 
;;; direct application
;;; 

(define-syntax-parameter direct-app? #f)

(define-syntax* (begin-direct stx)
  (syntax-parse stx
    [(_ e:expr ...+)
     #'(syntax-parameterize ([direct-app? #t])
         e ...)]))

(define-syntax* (define-my-syntax stx)
  (syntax-parse stx
    [(_ my-name:id monadic-impl:id direct-impl:id)
     #'(define-syntax (my-name ctx)
         (syntax-parse ctx
           [(_ . rest)
            (if (syntax-parameter-value #'direct-app?)
                #'(direct-impl . rest)
                #'(monadic-impl . rest))]))]))

;;;
;;; modbeg
;;; 

(define-syntax* (i2-module-begin stx)
  (make-module-begin 
   stx
   #:prelude-path #''(erda/i2-lib)))
