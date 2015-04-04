#lang racket/base

#|

|#

(require "i2-internal.rkt"
         "util.rkt" "util/provide-syntax.rkt"
         (rename-in magnolisp/surface 
                    [define mgl-define]
                    [declare mgl-declare])
         racket/bool racket/function racket/generic racket/list 
         racket/match racket/stxparam
         (for-syntax racket/base racket/function
                     racket/list racket/syntax
                     syntax/id-table syntax/parse
                     "util.rkt"))

(provide #%top #%expression
         
         ;; literals
         (rename-out [my-datum #%datum] [#%datum bare])
         
         ;; `require` forms
         require only-in except-in prefix-in rename-in combine-in
         relative-in only-meta-in
         local-require
         
         ;; `provide` forms
         provide all-defined-out all-from-out rename-out except-out
         prefix-out struct-out combine-out protect-out
         
         ;; `require` and `provide` forms
         for-syntax for-template for-label for-meta
         
         ;; sequences
         begin begin0
         
         ;; single static assignment forms
         let let* letrec

         ;; from Magnolisp
         type export foreign ;; annotations
         -> exists for-all <> ;; type expressions
         (rename-out [exists ∃] [for-all ∀])
         let-annotate cast ;; value expressions
         abstract-type ;; miscellaneous
         
         ;; (rename-out [my-app #%app]) ;; xxx
         (rename-out [#%plain-app #%app]) ;; xxx temporarily
         (from-prefixed-out my- quote 
                            if if-not or and cond
                            declare define)
         value)

;;; 
;;; expressions
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

(define (monadic-not v)
  (cond-or-fail
   ((Good? v) (Good (not (Good-v v))))
   ((Bad? v) (bad-condition #:bad-arg v #'monadic-not))))

;; We use this internally (only), to avoid (my-app not v).
(define-my-syntax my-not monadic-not not)

(define-syntax (monadic-if stx)
  (syntax-parse stx
    [(_ c:expr t:expr e:expr)
     #'(let-Good-args 
        ([v c]) #:op monadic-if
        #:then (if (Good-v v) t e))]))

(define-my-syntax my-if monadic-if if)

(define-syntax-rule (my-if-not c t e)
  (my-if c e t))

(define-syntax monadic-or
  (syntax-rules ()
    [(_) (Good #f)]
    [(_ e) e]
    [(_ e0 e ...)
     (let ([v e0])
       (monadic-if v v (monadic-or e ...)))]))

(define-my-syntax my-or monadic-or or)

(define-syntax monadic-and
  (syntax-rules ()
    [(_) (Good #t)]
    [(_ e) e]
    [(_ e0 e ...)
     (monadic-if e0 (monadic-and e ...) (Good #f))]))

(define-my-syntax my-and monadic-and and)

(define-syntax (monadic-cond stx)
  (syntax-parse stx #:datum-literals (else)
    [(_ [else e:expr]) #'e]
    [(_ [c:expr t:expr] e ...)
     #'(monadic-if c t (monadic-cond e ...))]))

(define-my-syntax my-cond monadic-cond cond)

;;; 
;;; monadic block
;;; 

(define-syntax* block
  (syntax-rules ()
    [(_ e) 
     e]
    [(_ (#:let x v) . es)
     (let ([x v])
       (block . es))]
    [(_ (#:when c #:let x v) . es)
     (let ([x (my-if c v x)])
       (if (Bad? x)
           x
           (block . es)))]
    [(_ e . es)
     (let () e (block . es))]
    ))

;;; 
;;; unwrapped operation
;;; 

;; The opposite of a `do` in Haskell. The `b ...` expressions deal in
;; bare values. The "free variables" and their values should be given
;; as `[p e] ...` for unwrapping.
(define-syntax* (anti-do stx)
  (syntax-parse stx
    [(_ ([p:id e:expr] ...) b:expr ...+)
     #'(let-Good-args 
        ([p e] ...) #:op anti-do
        #:then
        (let ([p (Good-v p)] ...)
          (let ([r (begin-direct b ...)])
            (if (data-invariant? r)
                (Good r)
                (bad-condition #:data-invariant 
                               (Good r) #'anti-do
                               (list (Good p) ...))))))]))

;;;
;;; declarations
;;;

(begin-for-syntax
  ;; Metadata for a function.
  (abstract-struct Function () #:transparent)
  
  ;; A function that is called directly, without any additional
  ;; processing. Metadata declared using `declare`.
  (concrete-struct DirectFunction Function () #:transparent)
  
  (concrete-struct AlertingFunction Function 
                    (modifs params alerts) #:transparent)
  
  ;; `alert-name` is a symbol
  (abstract-struct AlertSpec (alert-name) #:transparent)
  ;; `cond-expr` is syntax
  (concrete-struct PreCond AlertSpec (cond-expr) #:transparent)
  (concrete-struct PostCond AlertSpec (cond-expr) #:transparent)
  
  (define (mk-rec-stx info)
    (define kind (first info))
    (define name (second info))
    (case-or-fail kind
      [(pre post) #`(#,(if (eq? kind 'pre) #'PreCond #'PostCond)
                     '#,name
                     #'#,(if (third info)
                             #`(my-not #,(fourth info))
                             (fourth info)))]))
  
  ;; (free-id-table/c identifier? Function?)
  (define* fun-meta-table (make-free-id-table))

  (define-syntax-class alert-pre-clause
    #:description "precondition alert"
    #:attributes (info)
    #:datum-literals (pre-when pre-unless)
    (pattern
     (n:id pre-when c:expr)
     #:attr info (list 'pre (syntax-e #'n) #f #'c))
    (pattern
     (n:id pre-unless c:expr)
     #:attr info (list 'pre (syntax-e #'n) #t #'c)))

  (define-syntax-class alert-post-clause
    #:description "postcondition alert"
    #:attributes (info)
    #:datum-literals (post-when post-unless)
    (pattern
     (n:id post-when c:expr)
     #:attr info (list 'post (syntax-e #'n) #f #'c))
    (pattern
     (n:id post-unless c:expr)
     #:attr info (list 'post (syntax-e #'n) #t #'c)))

  (define-splicing-syntax-class alerts-spec
    #:description "alerts specification"
    #:attributes (alerts)
    (pattern
     (~seq #:alert ((~or pre:alert-pre-clause
                         post:alert-post-clause) ...))
     #:attr alerts (append (attribute pre.info)
                           (attribute post.info))))

  ;; The `alerts` attribute is a list of alert info elements (as
  ;; accepted by `mk-rec-stx`).
  (define-splicing-syntax-class maybe-alerts
    #:attributes (alerts)
    (pattern 
     (~optional a:alerts-spec)
     #:attr alerts (or (attribute a.alerts) '())))

  ;; A superficial match against optional Magnolisp annotations.
  (define-splicing-syntax-class mgl-annos?
    #:attributes ([annos 1])
    (pattern (~seq #:: an:expr) 
             #:with (annos ...) #'(#:: an))
    (pattern (~seq) 
             #:with (annos ...) '()))
  
  (define-splicing-syntax-class mgl-annos+alerts?
    #:attributes ([annos 1] alerts)
    (pattern
     (~seq ||:mgl-annos? al:maybe-alerts)
     #:attr alerts (attribute al.alerts)))

  (define (mk-reg-DirectFunction n-stx)
    (with-syntax ([n n-stx])
      #'(begin-for-syntax
          (free-id-table-set!
           fun-meta-table
           #'n (DirectFunction)))))
  
  (define (mk-reg-AlertingFunction n-stx modifs-lst
                                   params-stx alerts-lst)
    (with-syntax ([n n-stx]
                  [modifs #`'#,modifs-lst]
                  [params params-stx]
                  [(rec ...) (map mk-rec-stx alerts-lst)])
      #'(begin-for-syntax
          (free-id-table-set! 
           fun-meta-table 
           #'n 
           (AlertingFunction modifs #'params (list rec ...)))))))

;; A version of `declare` that records metadata for Erda, and also
;; invokes Magnolisp `declare` for recording metadata for Magnolisp.
(define-syntax (my-declare stx)
  (syntax-parse stx
    [(_ (n:id . ps) #:direct opts:mgl-annos?)
     (with-syntax ([reg-DirectFunction
                    (mk-reg-DirectFunction #'n)])
       #'(begin
           (mgl-declare (n . ps) opts.annos ...)
           reg-DirectFunction))]
    [(_ (n:id p:id ...) #:handler opts:mgl-annos+alerts?)
     (with-syntax ([reg-AlertingFunction
                    (mk-reg-AlertingFunction 
                     #'n '(handler) #'(p ...) (attribute opts.alerts))])
       #'(begin
           (mgl-declare (n p ...) opts.annos ...)
           reg-AlertingFunction))]
    [(_ (n:id p:id ...) opts:mgl-annos+alerts?)
     (with-syntax ([reg-AlertingFunction
                    (mk-reg-AlertingFunction 
                     #'n '(primitive) #'(p ...) (attribute opts.alerts))])
       #'(begin
           (mgl-declare (n p ...) opts.annos ...)
           reg-AlertingFunction))]
    [(_ #:type . rest)
     #'(mgl-declare #:type . rest)]
    ))

(define-syntax (my-define stx)
  (syntax-parse stx
    [(_ n:id e:expr)
     #'(define n e)]
    [(_ n:id #:: as:expr e:expr)
     #'(mgl-define n #:: as e)]
    [(_ #:type . rest)
     #'(mgl-define #:type . rest)]
    [(_ (n:id . ps) #:direct opts:mgl-annos? b:expr ...)
     (with-syntax ([reg-DirectFunction
                    (mk-reg-DirectFunction #'n)])
       #'(begin
           (mgl-define (n . ps) opts.annos ... b ...)
           reg-DirectFunction))]
    [(_ (n:id p:id ...) #:handler opts:mgl-annos+alerts? b:expr ...+)
     (with-syntax ([reg-AlertingFunction
                    (mk-reg-AlertingFunction
                     #'n '(handler) #'(p ...) 
                     (attribute opts.alerts))])
       #'(begin
           (mgl-define (n p ...) opts.annos ... b ...)
           reg-AlertingFunction))]
    [(_ (n:id p:id ...) opts:mgl-annos+alerts? b:expr ...+)
     (with-syntax ([reg-AlertingFunction
                    (mk-reg-AlertingFunction
                     #'n '(regular) #'(p ...) 
                     (attribute opts.alerts))])
       #'(begin
           (mgl-define (n p ...) opts.annos ... b ...)
           reg-AlertingFunction))]
    ))
