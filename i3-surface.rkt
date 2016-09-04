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
                     racket/list racket/pretty racket/syntax
                     syntax/parse
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
         (rename-out [my-datum #%datum] [my-app #%app] [my-apply apply]
                     [my-quote quote]
                     [my-if if] [my-and and] [my-or or] [my-cond cond]
                     [my-do do]
                     [my-lambda lambda] [my-lambda λ] [my-thunk thunk])
         begin begin0
         let let* letrec
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

(define-syntax (monadic-lambda stx)
  (syntax-case stx ()
    [(_ ps b ...)
     #`(Good
        #,(syntax/loc stx
            (lambda ps b ...)))]))

(define-my-syntax my-lambda monadic-lambda lambda)

(define-syntax-rule (my-thunk b ...)
  (my-lambda () b ...))

;;; 
;;; function application
;;; 

(define (wrap-primitive proc)
  (Good (procedure-rename
         (lambda wargs
           (monadic-apply-primitive proc wargs))
         (object-name proc))))

;; Calls an unknown primitive, which thus has no declared error
;; unformation, but unwrapping and wrapping needs to happen in monadic
;; mode. The `proc` argument must be a `procedure?`, whereas `wargs`
;; must be a list of wrapped arguments.
(define (monadic-apply-primitive proc wargs)
  (cond
    [(ormap Bad? wargs)
     (bad-condition #:bad-arg (wrap-primitive proc) wargs)]
    [else
     (define bargs (map Good-v wargs))
     (define bresult (apply proc bargs))
     (cond
       [(not (data-invariant? bresult))
        (bad-condition #:bad-result (wrap-primitive proc) wargs)]
       [else
        (Good bresult)])]))

(define (monadic-app-fun tgt . wargs)
  (cond
    [(procedure? tgt) ;; primitive
     (monadic-apply-primitive tgt wargs)]
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

(define-syntax-parameter on-alert-hook
  (syntax-rules ()
    [(_ _ e) e]))

(define-syntax (monadic-app stx)
  (syntax-parse stx
    [(_ fun:expr args:expr ...)
     (define/with-syntax app-expr
       #'(monadic-app-fun fun args ...))
     ;; Report bad `fun` as a `Bad` value.
     #'(on-alert-hook fun app-expr)]))

(define-my-syntax my-app monadic-app #%app)

(define (monadic-apply tgt wargs)
  (define args (Good-v wargs))
  ;;(writeln (cons tgt args))
  (apply monadic-app-fun tgt args))
  
(define-my-syntax my-apply monadic-apply apply)

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
         (monadic-apply-primitive fun (list e ...)))]))

;;; 
;;; `if` and other conditionals
;;; 

;; This function is a `#:handler` in the sense that it allows a bad
;; then or else expression, long as it is not used.
(define* if-then
  (monadic-lambda
   (c t-lam e-lam)
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
      (bad-condition #:bad-arg if-then (list c t-lam e-lam))])))

(define-syntax (monadic-if stx)
  (syntax-parse stx
    [(_ c:expr t:expr e:expr)
     #'(monadic-app if-then c (my-thunk t) (my-thunk e))]))

(define-my-syntax my-if monadic-if if)

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
  (syntax-parse stx
    [(_ [#:else e:expr]) #'e]
    [(_ [c:expr t:expr] e ...)
     #'(monadic-if c t (monadic-cond e ...))]))

(define-my-syntax my-cond monadic-cond cond)

;;; 
;;; function definition
;;; 

(begin-for-syntax
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

  (define-syntax-class alert-throws-clause
    #:description "exception thrown alert"
    #:attributes (info)
    #:datum-literals (on-throw)
    (pattern
     (n:id on-throw p:expr)
     #:attr info (list 'on-throw (syntax-e #'n) #'p)))

  (define-splicing-syntax-class alerts-spec
    #:description "alerts specification"
    #:attributes (alerts)
    (pattern
     (~seq #:alert ((~or pre:alert-pre-clause
                         post:alert-post-clause
                         throws:alert-throws-clause) ...))
     #:attr alerts (append (attribute pre.info)
                           (attribute post.info)
                           (attribute throws.info))))
  
  (define-splicing-syntax-class maybe-alerts
    #:attributes (alerts)
    (pattern 
     (~optional a:alerts-spec)
     #:attr alerts (or (attribute a.alerts) '())))

  ;; `alert-name` is a symbol
  (abstract-struct AlertSpec (alert-name) #:transparent)
  ;; `cond-expr` is syntax
  (concrete-struct PreCond AlertSpec (cond-expr) #:transparent)
  (concrete-struct PostCond AlertSpec (cond-expr) #:transparent)
  ;; `predicate-expr` is syntax
  (concrete-struct OnThrow AlertSpec (predicate-expr) #:transparent)

  (define (alert-spec->ast info)
    (define kind (first info))
    (define name (second info))
    (case-or-fail kind
      [(pre post) ((if (eq? kind 'pre) PreCond PostCond)
                   name
                   (if (third info)
                       #`(my-app not #,(fourth info))
                       (fourth info)))]
      [(on-throw) (OnThrow name (third info))])))

;; If an alert expression evaluates to a bad value, or if the
;; expression holds, then an alert is triggered.
(define (check-alert alert v)
  (and (or (not (Good? v)) (Good-v v)) alert))

;; Turns a non-alert-checking expression `e-stx` into an
;; alert-checking expression, by wrapping the necessary checks around
;; it. The `kind` argument is one of primitive, regular, handler. The
;; `args-stx` argument is an expression giving an argument list
;; (wrapped values, as used in alerts). The `alert-lst` argument is a
;; list of `AlertSpec`. The `stx` argument is lexical context and
;; error reporting context for the expression. The `fun-id` argument
;; is the name of the function to define. The `e-stx` argument is the
;; expression to compute the value of the function without alert
;; checks. The `params-stx` argument is syntax for parameters to check
;; for goodness, or #f if no such check is to be made.
(define-for-syntax (mk-alerting-expr e-stx stx kind
                                     fun-id args-stx alert-lst
                                     params-stx)
  (define pre-lst (filter PreCond? alert-lst))
  (define post-lst (filter PostCond? alert-lst))
  (define throw-lst (filter OnThrow? alert-lst))

  (define/with-syntax fun fun-id)
  (define/with-syntax args args-stx)

  ;; Check data invariant for primitives.
  (when (eq? kind 'primitive)
    (define/with-syntax br (generate-temporary 'br))
    (define/with-syntax e e-stx)
    (set! e-stx
          #'(let [(br e)]
              (if (data-invariant? br)
                  (Good br)
                  (bad-condition #:bad-result fun args)))))
  
  ;; Trap exceptions from primitives.
  (when (and (eq? kind 'primitive)
             (not (null? throw-lst)))
    (define/with-syntax e e-stx)
    (define/with-syntax (exc-clause ...)
      (for/list ((x throw-lst)) ;; of OnThrow
                #`[#,(OnThrow-predicate-expr x)
                   (lambda (exn)
                     (bad-condition #:exception-alert
                                    '#,(AlertSpec-alert-name x)
                                    fun args))]))
    (set! e-stx #'(with-handlers (exc-clause ...) e)))

  ;; Check for any post-condition alerts.
  (unless (null? post-lst)
    (define/with-syntax e e-stx)
    (define/with-syntax r (generate-temporary 'r))
    (define/with-syntax (chk ...)
      (for/list ([post post-lst]) ;; of PostCond
        #`(check-alert
           '#,(AlertSpec-alert-name post)
           #,(PostCond-cond-expr post))))
    (define/with-syntax (check-bad-r ...)
      (if (eq? kind 'handler)
          null
          (list #'[(Bad? r) r])))
    (set! e-stx
          #'(let ([r e])
              (syntax-parameterize ([value (make-rename-transformer #'r)])
                (cond
                  check-bad-r ...
                  [chk => (lambda (alert)
                            (bad-condition #:postcond-alert alert fun args r))]
                  ...
                  [else r])))))

  ;; Check for any pre-condition alerts.
  (unless (null? pre-lst)
    (define/with-syntax e e-stx)
    (define/with-syntax (chk ...)
      (for/list ([pre pre-lst]) ;; of PreCond
        #`(check-alert
           '#,(AlertSpec-alert-name pre)
           #,(PreCond-cond-expr pre))))
    (set! e-stx
          #'(cond
              [chk => (lambda (alert)
                        (bad-condition #:precond-alert alert fun args))]
              ...
              [else e])))

  ;; Check for `Bad` arguments, as specified by `params-stx`.
  (when params-stx
    (define/with-syntax e e-stx)
    (set! e-stx
          (syntax-parse params-stx
            [(p:id ...)
             #'(cond
                 [(or (Bad? p) ...)
                  (bad-condition #:bad-arg fun args)]
                 [else e])]
            [(p:id ... . rest:id)
             #'(cond
                 [(or (Bad? p) ... (ormap Bad? rest))
                  (bad-condition #:bad-arg fun args)]
                 [else e])]
            [rest:id
             #'(cond
                 [(ormap Bad? rest)
                  (bad-condition #:bad-arg fun args)]
                 [else e])])))
  
  e-stx)
  
(define-syntax (declare stx)
  (syntax-parse stx
    [(_ (n:id p ...) #:is tgt:id #:direct)
     (syntax/loc stx
       (define n
         (my-lambda (p ...)
           (#%app tgt p ...))))]
    [(_ (n:id p:id ...) #:is tgt:id opts:maybe-alerts)
     (define specs (map alert-spec->ast (attribute opts.alerts)))
     (define/with-syntax a-e
       (mk-alerting-expr #'(tgt (Good-v p) ...) stx 'primitive
                         #'n #'(list p ...) specs #'(p ...)))
     (quasisyntax/loc stx
       (define n
         #,(syntax/loc #'tgt
             (monadic-lambda (p ...) a-e))))]
    [(_ (n:id p:id ... . rest:id) #:is tgt:id opts:maybe-alerts)
     (define specs (map alert-spec->ast (attribute opts.alerts)))
     (define/with-syntax a-e
       (mk-alerting-expr #'(apply tgt (Good-v p) ... (map Good-v rest))
                         stx 'primitive
                         #'n #'(list* p ... rest) specs #'(p ... . rest)))
     (quasisyntax/loc stx
       (define n
         #,(syntax/loc #'tgt
             (monadic-lambda (p ... . rest) a-e))))]
    ))

(define-syntax (monadic-define stx)
  (syntax-parse stx
    [(_ n:id e:expr)
     (syntax/loc stx
       (define n e))]
    [(_ (n:id . p) #:direct b:expr ...+)
     (quasisyntax/loc stx
       (define n
         #,(syntax/loc #'n
             (monadic-lambda p b ...))))]
    [(_ (n:id p:id ...) #:handler opts:maybe-alerts b:expr ...+)
     (define specs (map alert-spec->ast (attribute opts.alerts)))
     (define/with-syntax a-e
       (mk-alerting-expr #'(begin b ...) stx 'handler
                         #'n #'(list p ...) specs #f))
     (quasisyntax/loc stx
       (define n
         #,(syntax/loc #'n
             (monadic-lambda (p ...) a-e))))]
    [(_ (n:id p:id ... . rest:id) #:handler opts:maybe-alerts b:expr ...+)
     (define specs (map alert-spec->ast (attribute opts.alerts)))
     (define/with-syntax a-e
       (mk-alerting-expr #'(begin b ...) stx 'handler
                         #'n #'(list* p ... rest) specs #f))
     (quasisyntax/loc stx
       (define n
         #,(syntax/loc #'n
             (monadic-lambda (p ... . rest) a-e))))]
    [(_ (n:id p:id ...) opts:maybe-alerts b:expr ...+)
     (define specs (map alert-spec->ast (attribute opts.alerts)))
     (define/with-syntax a-e
       (mk-alerting-expr #'(begin b ...) stx 'regular
                         #'n #'(list p ...) specs #'(p ...)))
     (quasisyntax/loc stx
       (define n
         #,(syntax/loc #'n
             (monadic-lambda (p ...) a-e))))]
    [(_ (n:id p:id ... . rest:id) opts:maybe-alerts b:expr ...+)
     (define specs (map alert-spec->ast (attribute opts.alerts)))
     (define/with-syntax a-e
       (mk-alerting-expr #'(begin b ...) stx 'regular
                         #'n #'(list* p ... rest) specs #'(p ... . rest)))
     (quasisyntax/loc stx
       (define n
         #,(syntax/loc #'n
             (monadic-lambda (p ... . rest) a-e))))]
    ))

;; Note the completely different syntax.
(define-my-syntax my-define monadic-define define)

;;; 
;;; recovery
;;; 

;; A recovery chaining form. Tries the `try-e` expressions by
;; evaluating them in order, choosing either the first non-bad result,
;; or alternatively defaults to the `fail-e` expression instead.
(define-syntax* (::> stx)
  (syntax-parse stx
    [(_ fail-e:expr)
     #'fail-e]
    [(_ try-e:expr . rest)
     #'(let ([v try-e])
         (if (Bad? v)
             (::> . rest)
             v))]))

;; A more traditional `try`/`#:catch` form.
(define-syntax* (try stx)
  (define-syntax-class catch 
    #:description "#:catch clause for a `try`"
    #:attributes (info)
    (pattern
     ((a:id ...) h:expr ...+)
     #:attr info (list (syntax->list #'(a ...))
                       (syntax->list #'(h ...)))))
  
  (define-syntax-class catch-all
    #:description "#:catch clause for a `try`"
    #:attributes (then)
    (pattern
     ((~datum _) h:expr ...+)
     #:attr then (syntax->list #'(h ...))))

  (syntax-parse stx
    [(_ b:expr ...+ #:catch cc:catch ... (~optional ec:catch-all))
     (define r-stx (generate-temporary 'r))
     (with-syntax ([r r-stx])
       (with-syntax 
         ([(catch-clause ...)
           (for/list ([info (attribute cc.info)])
             (define name-ids (first info))
             (define then-stx-lst (second info))
             (with-syntax ([(a ...) name-ids]
                           [(then ...) then-stx-lst])
               #'[(or (Result-contains-name? r (quote a)) ...)
                  then ...]))]
          [catch-all-clause
           (let ([then-stx-lst (attribute ec.then)])
             (if then-stx-lst
                 #`[else #,@then-stx-lst]
                 #'[else r]))])
         #'(let ([r (begin b ...)])
             (if (Good? r) r
                 (syntax-parameterize ([value
                                        (make-rename-transformer #'r)])
                   (cond
                     catch-clause ...
                     catch-all-clause))))))]))

;;; 
;;; error monadic sequencing
;;; 

(provide >>=)

;; Error monadic bind. Differs from monads in that `f` takes a wrapped
;; (but Good) value.
(monadic-define (>>= v f) ;; M a -> (M b -> M b) -> M b
  (monadic-app f v))

;; Syntactic sugar for `>>=`.
(define-syntax* (monadic-do stx)
  (syntax-parse stx #:datum-literals (<-)
    [(_ e:expr) 
     #'e]
    [(_ (x:id <- e:expr) rest ...+)
     #'(monadic-app >>= e (monadic-lambda (x) (monadic-do rest ...)))]
    [(_ e:expr rest ...+)
     #'(monadic-app >>= e (monadic-lambda (_) (monadic-do rest ...)))]
    ))

;; Note the completely different syntax.
(define-my-syntax my-do monadic-do do)
