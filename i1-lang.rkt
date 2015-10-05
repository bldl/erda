#lang racket/base

#|

The RVM language, without the reader and the standard library.

|#

(require "i1-internal.rkt"
         "util.rkt" 
         "util/provide-syntax.rkt" "util/racket-require.rkt"
         racket/bool racket/function racket/generic racket/list 
         racket/match racket/stxparam
         (for-syntax racket/base racket/function
                     racket/list racket/syntax
                     syntax/id-table syntax/parse
                     "util.rkt"))

(provide #%module-begin #%top #%expression
         
         ;; literals
         (rename-out [my-datum #%datum] [#%datum bare])
         
         ;; `require` forms
         require only-in except-in prefix-in rename-in combine-in
         relative-in only-meta-in submod
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
         
         (rename-out [my-app #%app])
         not
         (from-prefixed-out my- quote if if-not or and cond define)
         value
         (all-from-out "util/racket-require.rkt"))

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

;; The `alert` field contains an alert name (a symbol).
(struct GotException (alert))

(define-syntax-rule (make-Bad-from-exception got op args)
  (bad-condition #:exception-alert (GotException-alert got) #'op args))

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
  ;; `predicate-expr` is syntax
  (concrete-struct OnThrow AlertSpec (predicate-expr) #:transparent)
  
  (define (mk-rec-stx info)
    (define kind (first info))
    (define name (second info))
    (case-or-fail kind
      [(pre post) #`(#,(if (eq? kind 'pre) #'PreCond #'PostCond)
                     '#,name
                     #'#,(if (third info)
                             #`(my-not #,(fourth info))
                             (fourth info)))]
      [(on-throw) #`(OnThrow '#,name #'#,(third info))]))
  
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

  (define (mk-reg-DirectFunction n-stx)
    (with-syntax ([n n-stx])
      #'(begin-for-syntax
          (free-id-table-set!
           fun-meta-table
           #'n (DirectFunction)))))
  
  (define (mk-reg-AlertingFunction n-stx modifs-lst
                                   params-stx alerts-lst)
    (with-syntax ([modifs #`'#,modifs-lst]
                  [params params-stx]
                  [(rec ...) (map mk-rec-stx alerts-lst)])
      #`(begin-for-syntax
          (free-id-table-set! 
           fun-meta-table 
           #'#,n-stx
           (AlertingFunction modifs #'params (list rec ...)))))))

(define-syntax (my-define stx)
  (syntax-parse stx
    [(_ n:id e:expr)
     #'(define n e)]
    [(_ (n:id p:id ...) opts:maybe-alerts b:expr ...+)
     (with-syntax ([reg-AlertingFunction
                    (mk-reg-AlertingFunction
                     #'n '(regular) #'(p ...) 
                     (attribute opts.alerts))])
       #'(begin
           (define (n p ...) b ...)
           reg-AlertingFunction))]
    [(_ (n:id p:id ...) #:handler opts:maybe-alerts b:expr ...+)
     (with-syntax ([reg-AlertingFunction
                    (mk-reg-AlertingFunction
                     #'n '(handler) #'(p ...) 
                     (attribute opts.alerts))])
       #'(begin
           (define (n p ...) b ...)
           reg-AlertingFunction))]
    [(_ (n:id p:id ...) #:direct b:expr ...+)
     (with-syntax ([reg-DirectFunction
                    (mk-reg-DirectFunction #'n)])
       #'(begin
           (define (n p ...) b ...)
           reg-DirectFunction))]))

(define-syntax* (declare stx)
  (syntax-parse stx
    [(_ (n:id p ...) #:direct)
     (mk-reg-DirectFunction #'n)]
    [(_ (n:id p:id ...) opts:maybe-alerts)
     ;;(writeln `(storing ,(syntax-e #'n) ,#'n ,(identifier-binding #'n) ,(syntax-local-phase-level)))
     (mk-reg-AlertingFunction 
      #'n '(primitive) #'(p ...) (attribute opts.alerts))]))

;; For debugging only. Prints out function metadata.
(define-syntax* (declarations-for stx)
  (syntax-parse stx
    [(_ n:id ...)
     (for ([n-stx (syntax->list #'(n ...))])
       (write `(declarations-for : ,n-stx ,(free-id-table-ref 
                                            fun-meta-table n-stx
                                            (lambda () 'undeclared))))
       (newline))
     #'(begin)]))

;; Produces a Bad value if a precondition does not hold (or its
;; checking fails). Otherwise returns #f. The `f-stx` identifier names
;; the operation, the `name` symbol specifies an alert name for the
;; precondition, where as `v` is the result of the precondition
;; expression (negated as necessary, so that a true value indicates
;; that the condition does not hold).
(define (maybe-get-pre-failure f-stx name v args)
  (cond
   [(Bad? v) (bad-condition #:bad-precond v f-stx args)]
   [else
    (define gv (Good-v v))
    (unless (boolean? gv)
      (error 'maybe-get-pre-failure "expected boolean, got ~s" gv))
    (and gv (bad-condition #:precond-alert name f-stx args))]))

;; As for `maybe-get-pre-failure`, but concerns a postcondition, and
;; the additional argument `r` is a wrapped good result for the
;; operation.
(define (maybe-get-post-failure f-stx r name v args)
  (cond
   [(Bad? v) (bad-condition #:bad-postcond v f-stx r args)]
   [else
    (define gv (Good-v v))
    (unless (boolean? gv)
      (error 'maybe-get-post-failure "expected boolean, got ~s" gv))
    (and gv (bad-condition #:postcond-alert name f-stx r args))]))

(define-for-syntax (mk-UndeclaredFunction-app stx f-stx args-stx)
  (define arg-lst (syntax->list args-stx))
  (define param-lst (generate-temporaries arg-lst))
  (with-syntax ([f f-stx]
                [(a ...) arg-lst]
                [(p ...) param-lst])
    #'(let-Good-args
       ([p a] ...) #:op f
       #:then
       (let ([r (#%plain-app f (Good-v p) ...)])
         (cond
          ((data-invariant? r)
           (Good r))
          (else
           (bad-condition #:data-invariant (Good r) #'f (list p ...))))))))
  
(define-for-syntax (mk-AlertingFunction-app info stx f-stx args-stx)
  (define arg-lst (syntax->list args-stx))
  (define modifs (AlertingFunction-modifs info))

  ;; `param-lst` lists the variables to which to bind arguments.
  ;; `pre-lst` is a list of PreCond objects, etc.
  (define-values (param-lst pre-lst post-lst throw-lst)
    (if info
        (let* ((p-stx (AlertingFunction-params info))
               (p-lst (syntax->list p-stx))
               (alert-lst (AlertingFunction-alerts info))
               (pre-lst (filter PreCond? alert-lst))
               (post-lst (filter PostCond? alert-lst))
               (throw-lst (filter OnThrow? alert-lst))
               (param-decl? (not (and (null? pre-lst)
                                      (null? post-lst)))))
          (values
           (if param-decl?
               (let ()
                 (unless (= (length p-lst) (length arg-lst))
                   (error 'my-app
                          "declared arity for function ~a is ~a: ~s"
                          (syntax-e f-stx) (length p-lst) stx))
                 p-lst)
               (generate-temporaries arg-lst))
           pre-lst post-lst throw-lst))
        (values (generate-temporaries arg-lst) '() '() '())))

  (define r-stx (generate-temporary 'r))

  (with-syntax
    ([f f-stx]
     [(a ...) arg-lst]
     [(p ...) param-lst]
     [r r-stx])
    (with-syntax
      ([(pre-check ...)
        (if (null? pre-lst)
            null
            (list
             (with-syntax*
               ([args (generate-temporary 'args)]
                [(get-fail ...)
                 (for/list ([pre pre-lst]) ;; of PreCond
                   #`(maybe-get-pre-failure
                      #'f
                      '#,(AlertSpec-alert-name pre)
                      #,(PreCond-cond-expr pre)
                      args))])
               #'[(let ([args (list p ...)])
                    (or get-fail ...)) => (lambda (x) x)])))]
       [(exc-clause ...)
        (for/list ((x throw-lst)) ;; of OnThrow
          #`[#,(OnThrow-predicate-expr x)
              (lambda (exn) 
                (GotException '#,(AlertSpec-alert-name x)))])]
       [(exc-check ...)
        (if (null? throw-lst)
            null
            (list
             #'[(GotException? r)
                (make-Bad-from-exception r f (list p ...))]))]
       [post-checked-r
        (if (null? post-lst)
            r-stx
            (with-syntax*
              ([args (generate-temporary 'args)]
               [(get-fail ...)
                (for/list ([post post-lst]) ;; of PostCond
                  #`(maybe-get-post-failure
                     #'f r
                     '#,(AlertSpec-alert-name post)
                     #,(PostCond-cond-expr post)
                     args))])
              #'(let ([args (list p ...)])
                  (syntax-parameterize ([value
                                         (make-rename-transformer #'r)])
                    (cond
                      [(or get-fail ...) => (lambda (x) x)]
                      [else r])))))])
      (cond-or-fail
       [(memq 'primitive modifs)
        #'(let-Good-args 
           ([p a] ...) #:op f
           #:then
           (cond 
             pre-check ...
             [else
              (let ([r (with-handlers (exc-clause ...)
                         (#%plain-app f (Good-v p) ...))])
                (cond
                  exc-check ...
                  [(not (data-invariant? r))
                   (bad-condition #:data-invariant 
                                  (Good r) #'f (list p ...))]
                  [else
                   (let ([r (Good r)])
                     post-checked-r)]))]))]
       [(memq 'regular modifs)
        #'(let-Good-args 
           ([p a] ...) #:op f
           #:then
           (cond 
             pre-check ...
             [else
              (let ([r (with-handlers (exc-clause ...)
                         (#%plain-app f p ...))])
                (cond
                  exc-check ...
                  [(Bad? r)
                   r]
                  [else
                   post-checked-r]))]))]
       [(memq 'handler modifs)
        #'(let ([p a] ...)
            (cond 
              pre-check ...
              [else
               (let ([r (with-handlers (exc-clause ...)
                          (#%plain-app f p ...))])
                 (cond
                   exc-check ...
                   [else
                    ;; Any predicates used in post-conditions really
                    ;; should be `#:handler`s also.
                    post-checked-r]))]))]))))

(define-syntax-parameter on-alert-hook
  (syntax-rules ()
    [(_ _ e) e]))

(define-syntax (monadic-app stx)
  (syntax-parse stx
    [(_ f:id a:expr ...)
     (define f-stx #'f)
     (define info (free-id-table-ref fun-meta-table f-stx #f))
     ;;(writeln `(querying ,(syntax-e f-stx) ,f-stx ,(identifier-binding f-stx) ,(syntax-local-phase-level) ,info))
     (with-syntax 
       ([app-expr
         (cond-or-fail
          [(not info)
           (mk-UndeclaredFunction-app stx f-stx #'(a ...))]
          [(DirectFunction? info)
           #'(#%app f a ...)]
          [(AlertingFunction? info)
           (mk-AlertingFunction-app info stx f-stx #'(a ...))])])
       #'(on-alert-hook f app-expr))]))

(define-my-syntax my-app monadic-app #%app)

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
               #'[(let ([n (Bad-origin-name r)])
                    (or (eq? n (quote a)) ...))
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

;; Handles any error in the `try-e` expression by evaluating the
;; `fail-e` expression instead. Equivalent to (try try-e #:catch [_
;; fail-e]).
(define-syntax* (default stx)
  (syntax-parse stx
    [(_ try-e fail-e)
     #'(let ([v try-e])
         (if (Bad? v)
             fail-e
             v))]))

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
