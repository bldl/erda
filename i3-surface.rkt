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
                     syntax/id-set syntax/parse
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
                     [my-quote quote]
                     [my-if if] [my-and and] [my-or or] [my-cond cond]
                     [my-when when] [my-unless unless]
                     [my-lambda lambda] [my-lambda λ] [my-thunk thunk])
         begin begin0
         let let* letrec
         value)

;;; 
;;; known function tracking
;;;

;; (free-id-set/c identifier? #:mutability 'mutable)
(define-for-syntax known-fun-set (mutable-free-id-set))

(define-syntax-rule (set-known-fun! fun)
  (begin-for-syntax
    (free-id-set-add! known-fun-set #'fun)))

(define-syntax (define-known stx)
  (syntax-parse stx
    [(_ (n:id . ps) . rest)
     #'(begin
         (define (n . ps) . rest)
         (set-known-fun! n))]
    [(_ n:id . rest)
     #'(begin
         (define n . rest)
         (set-known-fun! n))]))

(define-syntax define-known*
  (syntax-rules ()
    [(_ (id . ps) . rest)
     (begin
       (define-known (id . ps) . rest)
       (provide id))]
    [(_ id . rest)
     (begin
       (define-known id . rest)
       (provide id))]))

(define-syntax-rule
  (Good-lambda n ps body ...)
  (Good (procedure-rename (lambda ps body ...) n))) 

(define-syntax-rule
  (define-known-Good-lambda* n ps body ...)
  (define-known* n
    (Good-lambda 'n ps
      body ...)))

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

;;; 
;;; alert checking
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
                 [(or (Bad? p) ... (ormap Bad? (Good-v rest)))
                  (bad-condition #:bad-arg fun args)]
                 [else e])]
            [rest:id
             #'(cond
                 [(ormap Bad? (Good-v rest))
                  (bad-condition #:bad-arg fun args)]
                 [else e])])))
  
  e-stx)
  
;;; 
;;; function application
;;;

(define (wrap-primitive proc)
  (Good-lambda (object-name proc) wargs
    (monadic-apply-primitive proc wargs)))

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
    [(_ fun:id args:expr ...)
     (define/with-syntax app-expr
       (if (free-id-set-member? known-fun-set #'fun)
           #'((Good-v fun) args ...) ;; direct call to a known function
           #'(monadic-app-fun fun args ...)))
     #'(on-alert-hook fun app-expr)]
    [(_ fun:expr args:expr ...)
     #'(monadic-app-fun fun args ...)]))

(define-my-syntax my-app monadic-app #%app)

;;; 
;;; `lambda` and `thunk`
;;;

;; Wraps the function value and any rest argument in `Good`.
(define-syntax (plain-lambda stx)
  (syntax-parse stx
    [(_ (p:id ...) b ...)
     #`(Good
        #,(syntax/loc stx
            (lambda (p ...) b ...)))]
    [(_ (p:id ... . ps:id) b ...)
     (define/with-syntax in-ps (generate-temporary 'rest))
     #`(Good
        #,(syntax/loc stx
            (lambda (p ... . in-ps)
              (let ([ps (Good in-ps)])
                b ...))))]
    [(_ ps:id b ...)
     (define/with-syntax in-ps (generate-temporary 'rest))
     #`(Good
        #,(syntax/loc stx
            (lambda in-ps
              (let ([ps (Good in-ps)])
                b ...))))]))

(define-syntax (monadic-lambda stx)
  (syntax-parse stx
    [(_ ps #:direct b:expr ...+)
     (syntax/loc stx
       (plain-lambda ps b ...))]
    [(_ ps #:handler b:expr ...+) ;; optimize no-alerts case
     (syntax/loc stx
       (plain-lambda ps b ...))]
    [(_ (p:id ... . rest:id) #:handler opts:alerts-spec b:expr ...+)
     (define specs (map alert-spec->ast (attribute opts.alerts)))
     (define lam-id (generate-temporary 'lambda))
     (define/with-syntax a-e
       (mk-alerting-expr #'(begin b ...) stx 'handler
                         lam-id #'(list* p ... (Good-v rest))
                         specs #f))
     (define/with-syntax n lam-id)
     (syntax/loc stx
       (letrec ([n (plain-lambda (p ... . rest) a-e)])
         n))]
    [(_ (p:id ...) #:handler opts:alerts-spec b:expr ...+)
     (define specs (map alert-spec->ast (attribute opts.alerts)))
     (define lam-id (generate-temporary 'lambda))
     (define/with-syntax a-e
       (mk-alerting-expr #'(begin b ...) stx 'handler
                         lam-id #'(list p ...) specs #f))
     (define/with-syntax n lam-id)
     (syntax/loc stx
       (letrec ([n (plain-lambda (p ...) a-e)])
         n))]
    [(_ ps:id #:handler opts:alerts-spec b:expr ...+)
     (define specs (map alert-spec->ast (attribute opts.alerts)))
     (define lam-id (generate-temporary 'lambda))
     (define/with-syntax a-e
       (mk-alerting-expr #'(begin b ...) stx 'handler
                         lam-id #'(Good-v ps)
                         specs #f))
     (syntax/loc stx
       (letrec ([n (plain-lambda ps a-e)])
         n))]
    [(_ (p:id ...) #:primitive opts:maybe-alerts b:expr ...+)
     (define specs (map alert-spec->ast (attribute opts.alerts)))
     (define lam-id (generate-temporary 'lambda))
     (define body-stx
       #'(let ([p (Good-v p)] ...)
           b ...))
     (define/with-syntax a-e
       (mk-alerting-expr body-stx stx 'primitive
                         lam-id #'(list p ...) specs #'(p ...)))
     (define/with-syntax n lam-id)
     (syntax/loc stx
       (letrec ([n (plain-lambda (p ...) a-e)])
         n))]
    [(_ () b:expr ...+) ;; optimize no-args-or-alerts case
     (syntax/loc stx
       (plain-lambda () b ...))]
    [(_ (p:id ... . rest:id) opts:maybe-alerts b:expr ...+) ;; regular
     (define specs (map alert-spec->ast (attribute opts.alerts)))
     (define lam-id (generate-temporary 'lambda))
     (define/with-syntax a-e
       (mk-alerting-expr #'(begin b ...) stx 'regular
                         lam-id #'(list* p ... (Good-v rest))
                         specs #'(p ... . rest)))
     (define/with-syntax n lam-id)
     (syntax/loc stx
       (letrec ([n (plain-lambda (p ... . rest) a-e)])
         n))]
    [(_ (p:id ...) opts:maybe-alerts b:expr ...+) ;; regular
     (define specs (map alert-spec->ast (attribute opts.alerts)))
     (define lam-id (generate-temporary 'lambda))
     (define/with-syntax a-e
       (mk-alerting-expr #'(begin b ...) stx 'regular
                         lam-id #'(list p ...) specs #'(p ...)))
     (define/with-syntax n lam-id)
     (syntax/loc stx
       (letrec ([n (plain-lambda (p ...) a-e)])
         n))]
    [(_ ps:id opts:maybe-alerts b:expr ...+) ;; regular
     (define specs (map alert-spec->ast (attribute opts.alerts)))
     (define lam-id (generate-temporary 'lambda))
     (define/with-syntax a-e
       (mk-alerting-expr #'(begin b ...) stx 'regular
                         lam-id #'(Good-v ps)
                         specs #'ps))
     (syntax/loc stx
       (letrec ([n (plain-lambda ps a-e)])
         n))]
    ))

(define-my-syntax my-lambda monadic-lambda lambda)

(define-syntax-rule (my-thunk b ...)
  (my-lambda () b ...))

;;; 
;;; function definition
;;; 

(define (Good-procedure-rename proc name)
  (Good (procedure-rename (Good-v proc) name)))

(define-syntax (declare stx)
  (syntax-parse stx
    [(_ (n:id p ...) #:is tgt:id #:direct)
     (syntax/loc stx
       (define-known n
         (Good (procedure-rename tgt 'n))))]
    [(_ (n:id p:id ...) #:is tgt:id opts:maybe-alerts)
     (define specs (map alert-spec->ast (attribute opts.alerts)))
     (define/with-syntax a-e
       (mk-alerting-expr #'(tgt (Good-v p) ...) stx 'primitive
                         #'n #'(list p ...) specs #'(p ...)))
     (define/with-syntax lam
       (syntax/loc #'tgt
         (plain-lambda (p ...) a-e)))
     (quasisyntax/loc stx
       (define-known n
         (Good-procedure-rename lam 'n)))]
    [(_ (n:id p:id ... . rest:id) #:is tgt:id opts:maybe-alerts)
     (define specs (map alert-spec->ast (attribute opts.alerts)))
     (define/with-syntax a-e
       (mk-alerting-expr #'(apply tgt (Good-v p) ... (map Good-v (Good-v rest)))
                         stx 'primitive
                         #'n #'(list* p ... (Good-v rest))
                         specs #'(p ... . rest)))
     (define/with-syntax lam
       (syntax/loc #'tgt
         (plain-lambda (p ... . rest) a-e)))
     (quasisyntax/loc stx
       (define-known n
         (Good-procedure-rename lam 'n)))]
    ))

(define-syntax (monadic-define stx)
  (syntax-parse stx
    [(_ n:id e:expr)
     (syntax/loc stx
       (define n e))]
    [(_ (n:id . p) #:direct b:expr ...+)
     (define/with-syntax lam
       (syntax/loc #'n
         (plain-lambda p b ...)))
     (quasisyntax/loc stx
       (define-known n
         (Good-procedure-rename lam 'n)))]
    [(_ (n:id p:id ...) #:handler opts:maybe-alerts b:expr ...+)
     (define specs (map alert-spec->ast (attribute opts.alerts)))
     (define/with-syntax a-e
       (mk-alerting-expr #'(begin b ...) stx 'handler
                         #'n #'(list p ...) specs #f))
     (define/with-syntax lam
       (syntax/loc #'n
         (plain-lambda (p ...) a-e)))
     (quasisyntax/loc stx
       (define-known n
         (Good-procedure-rename lam 'n)))]
    [(_ (n:id p:id ... . rest:id) #:handler opts:maybe-alerts b:expr ...+)
     (define specs (map alert-spec->ast (attribute opts.alerts)))
     (define/with-syntax a-e
       (mk-alerting-expr #'(begin b ...) stx 'handler
                         #'n #'(list* p ... (Good-v rest)) specs #f))
     (define/with-syntax lam
       (syntax/loc #'n
         (plain-lambda (p ... . rest) a-e)))
     (quasisyntax/loc stx
       (define-known n
         (Good-procedure-rename lam 'n)))]
    [(_ (n:id p:id ...) opts:maybe-alerts b:expr ...+)
     (define specs (map alert-spec->ast (attribute opts.alerts)))
     (define/with-syntax a-e
       (mk-alerting-expr #'(begin b ...) stx 'regular
                         #'n #'(list p ...) specs #'(p ...)))
     (define/with-syntax lam
       (syntax/loc #'n
         (plain-lambda (p ...) a-e)))
     (quasisyntax/loc stx
       (define-known n
         (Good-procedure-rename lam 'n)))]
    [(_ (n:id p:id ... . rest:id) opts:maybe-alerts b:expr ...+)
     (define specs (map alert-spec->ast (attribute opts.alerts)))
     (define/with-syntax a-e
       (mk-alerting-expr #'(begin b ...) stx 'regular
                         #'n #'(list* p ... (Good-v rest))
                         specs #'(p ... . rest)))
     (define/with-syntax lam
       (syntax/loc #'n
         (plain-lambda (p ... . rest) a-e)))
     (quasisyntax/loc stx
       (define-known n
         (Good-procedure-rename lam 'n)))]
    ))

;; Note the completely different syntax.
(define-my-syntax my-define monadic-define define)

;;; 
;;; on-alert
;;;

(define-for-syntax (as-syntax x)
  (cond
    ((syntax? x) #`(quote-syntax #,x))
    ((list? x) #`(list #,@(map as-syntax x)))
    (else (raise-argument-error
           'as-syntax "(or/c syntax? list?)" x))))

;; As an optimization, this macro only expands to a call to the
;; recovery function `recover` where the operation ID `op-id` of the
;; operation expression `op-e` is on the list of IDs `(can-id ...)`
;; supported by the recovery function.
(define-syntax (maybe-recover stx)
  (syntax-parse stx
    [(_ (can-id:id ...+) recover:id op-id:id op-e:expr)
     (cond
       [(ormap (curry free-identifier=? #'op-id)
               (syntax->list #'(can-id ...)))
        #'(recover #'op-id op-e)]
       [else
        #'op-e])]))

(define-syntax-parameter on-alert-lst '())

(define-syntax* (on-alert stx)
  (define-syntax-class clause
    #:description "handler clause"
    #:attributes (info)
    (pattern
     [(op:id ...) hnd:expr ...+]
     #:attr info (list (syntax->list #'(op ...))
                       (syntax->list #'(hnd ...)))))

  ;; Produces syntax for an anonymous function which might do some
  ;; context-sensitive recovery on a Bad input value.
  (define (make-recover-lam lst)
    ;;(printf "`on-alert` handlers: ~s~n" lst)
    (with-syntax ([bad (generate-temporary 'bad)]
                  [(cond-clause ...)
                   (for/list ([spec lst])
                     (with-syntax ([(decl-op ...) (first spec)]
                                   [(action ...) (second spec)])
                       #'[(or (free-identifier=? op-id #'decl-op) ...)
                          action ...]))])
      (define r
        #'(lambda (op-id bad)
            (syntax-parameterize ([value
                                   (make-rename-transformer #'bad)])
              (cond
                [(Good? bad) bad]
                cond-clause ...
                [else bad]))))
      ;;(pretty-print (syntax->datum r))
      r))
  
  (syntax-parse stx
    ((_ (c:clause ...) e:expr ...+)
     (define lst (append
                  (reverse (attribute c.info))
                  (syntax-parameter-value #'on-alert-lst)))
     (cond
       ((null? lst)
        #'(let () e ...))
       (else
        (with-syntax ([new-lst (as-syntax lst)]
                      [(can-id ...) (apply append (map first lst))]
                      [recover-lam (make-recover-lam lst)])
          #'(let ([recover recover-lam])
              (syntax-parameterize ([on-alert-lst new-lst]
                                    [on-alert-hook
                                     (syntax-rules ()
                                       [(_ op-id op-e)
                                        (maybe-recover 
                                         (can-id ...) recover 
                                         op-id op-e)])])
                e ...))))))))

;;; 
;;; direct mode
;;; 

(define-syntax* (direct-lambda stx)
  (syntax-parse stx
    [(_ ps opts:maybe-alerts b:expr ...+)
     (define/with-syntax (alert ...) (attribute opts))
     (syntax/loc stx
       (monadic-lambda ps
         #:primitive alert ...
         (begin-direct b ...)))]))

(define-syntax* (define-direct stx)
  (syntax-parse stx
    [(_ (n:id p:id ...) . rest)
     (syntax/loc stx
       (define-known n
         (direct-lambda (p ...) . rest)))]))

;; A `let-direct` with support for alert declarations.
(define-syntax* (let-direct+ stx)
  (syntax-parse stx
    [(_ ([p:id e:expr] ...) opts:maybe-alerts b:expr ...+)
     (define/with-syntax (alert ...) (attribute opts))
     (syntax/loc stx
        ;; We need a function value in any case for history.
       (monadic-app
        (direct-lambda (p ...)
          alert ... b ...)
        e ...))]))

;; Enables direct mode for a block scope. The `b ...` expressions deal
;; in bare values, except that native Erda functions work as usual.
;; The "free variables" and their values should be given as `[p e]
;; ...` for unwrapping.
(define-syntax* (let-direct stx)
  (syntax-parse stx
    [(_ ([p:id e:expr] ...) b:expr ...+)
     (syntax/loc stx
       (let-direct+ ([p e] ...) b ...))]))

;;; 
;;; `if` and other conditionals
;;; 

(provide if-then)

;; This function is a `#:handler` in the sense that it allows a bad
;; then or else expression, long as it is not used.
(define-known if-then
  (Good-procedure-rename
   (plain-lambda
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
       (bad-condition #:bad-arg if-then (list c t-lam e-lam))]))
   'if-then))

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
    [(_ [#:else e:expr ...+]) #'(begin e ...)]
    [(_ [c:expr t:expr ...+] . rest)
     #'(monadic-if c (begin t ...) (monadic-cond . rest))]))

(define-my-syntax my-cond monadic-cond cond)

(define-syntax-rule
  (monadic-when c t ...)
  (monadic-if c (let () t ...) (void)))

(define-my-syntax my-when monadic-when when)

(define-syntax-rule
  (monadic-unless c t ...)
  (monadic-if c (void) (let () t ...)))

(define-my-syntax my-unless monadic-unless unless)

;;; 
;;; `apply`
;;; 

(provide (rename-out [monadic-apply apply]))

(define-known-Good-lambda* function? (x)
  (Good
   (or (and (Good? x) (procedure? (Good-v x)))
       (procedure? x))))

;; Contorted, to also allow `tgt` to be an unwrapped procedure.
(define-known monadic-apply
  (Good-lambda 'apply (tgt wargs)
    (cond
      [(Good? wargs)
       (apply monadic-app-fun tgt (Good-v wargs))]
      [(procedure? tgt)
       ;; Can call with a "bare" procedure, but `wargs` was bad.
       (bad-condition #:bad-arg (wrap-primitive tgt) wargs)]
      [else
       (bad-condition #:bad-arg tgt wargs)])))
  
;;; 
;;; recovery
;;; 

(provide :-:> :/:>)

;; Good expression chaining. Sets `value` for the continuation.
(define-syntax (:-:> stx)
  (syntax-parse stx
    [(_ e:expr)
     #'e]
    [(_ try-e:expr . rest)
     #'(let ([r try-e])
         (if (Good? r)
             (syntax-parameterize ([value
                                    (make-rename-transformer #'r)])
               (:-:> . rest))
             r))]))

;; Bad expression chaining. Sets `value` for the continuation.
(define-syntax (:/:> stx)
  (syntax-parse stx
    [(_ e:expr)
     #'e]
    [(_ try-e:expr . rest)
     #'(let ([r try-e])
         (if (Bad? r)
             (syntax-parameterize ([value
                                    (make-rename-transformer #'r)])
               (:/:> . rest))
             r))]))

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

  (define-splicing-syntax-class no-catch
    #:description "#:else clause for a `try`"
    #:attributes (then)
    (pattern
     (~seq #:else h:expr ...+)
     #:attr then (syntax->list #'(h ...))))

  (syntax-parse stx
    [(_ b:expr ...+ #:catch cc:catch ...
        (~optional ec:catch-all)
        (~optional nc:no-catch))
     (define/with-syntax r (generate-temporary 'r))
     (define/with-syntax (catch-clause ...)
       (for/list ([info (attribute cc.info)])
         (define name-ids (first info))
         (define then-stx-lst (second info))
         (with-syntax ([(a ...) name-ids]
                       [(then ...) then-stx-lst])
           #'[(or (Result-contains-name? r (quote a)) ...)
              then ...])))
     (define/with-syntax catch-all-clause
       (let ([then-stx-lst (attribute ec.then)])
         (if then-stx-lst
             #`[else #,@then-stx-lst]
             #'[else r])))
     (define/with-syntax good-then
       (let ([then-stx-lst (attribute nc.then)])
         (if then-stx-lst
             #`(let () #,@then-stx-lst)
             #'r)))
     #'(let ([r (let () b ...)])
         (syntax-parameterize ([value
                                (make-rename-transformer #'r)])
           (if (Good? r)
               good-then
               (cond
                 catch-clause ...
                 catch-all-clause))))]))

;;; 
;;; alert and documentation "contracts"
;;;

(define-known-Good-lambda* result-of? (f? x)
  (Good ((Result/c f?) x)))

(define-known-Good-lambda* good-result-of? (f? x)
  (Good ((Good/c f?) x)))

;; Takes a primitive predicate `f?`, returning a wrapped value
;; processing predicate that applies the primitive predicate inside
;; any good value. The primitive need not hold for a bad value.
(define-known-Good-lambda* result/e (f?)
  (define g? (Result/c f?))
  (Good
   (lambda (x)
     (Good (g? x)))))

;; Like `result/e`, but produces a predicate that does not hold for
;; bad values.
(define-known-Good-lambda* good-result/e (f?)
  (define g? (Good/c f?))
  (Good
   (lambda (x)
     (Good (g? x)))))
