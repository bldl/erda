#lang racket/base

#|

About this language:
- Racket module system is supported (imports and exports)
- Racket macros or compile-time code is not supported
- (quasi)quoting of data is not supported in general (symbols are)
- only first-class function application is supported
- only single static assignment is supported (no `set!`)
- all expressions evaluate to a single value (no `values`)
- accordingly, there are no multi-variable binding forms
- there is no unit type (and no `void`, `when`, or `unless`)

For R&D support, we do allow Racket to be used:
- `require` may be used to bring in Racket names globally
- `local-require` may be used to do the same locally
- `let-racket-*` forms are convenient for giving Racket expressions
- `bare` may be used to give unwrapped Racket literals
With great power comes great responsibility not to break assumptions.

|#

(require "i1-internal.rkt"
         "util.rkt" 
         "util/provide-syntax.rkt" "util/racket-require.rkt"
         racket/bool racket/function racket/generic racket/list 
         racket/match racket/stxparam
         (for-syntax racket/base racket/list racket/syntax
                     syntax/id-table syntax/parse
                     "util.rkt"))

(provide #%module-begin #%top #%expression
         
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
         
         (rename-out [my-app #%app])
         not
         (from-prefixed-out my- quote if if-not or and cond define)
         (all-from-out "util/racket-require.rkt"))

(define-syntax-parameter direct-app? #f)

(define-syntax* (begin-direct stx)
  (syntax-parse stx
    [(_ e:expr ...+)
     #'(syntax-parameterize ([direct-app? #t])
         e ...)]))

(define-syntax (define-my-syntax stx)
  (syntax-parse stx
    [(_ my-name:id monadic-impl:id direct-impl:id)
     #'(define-syntax (my-name ctx)
         (syntax-parse ctx
           [(_ . rest)
            (if (syntax-parameter-value #'direct-app?)
                #'(direct-impl . rest)
                #'(monadic-impl . rest))]))]))

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

(struct GotException (alert))

(define-syntax (make-Bad stx)
  (syntax-parse stx
    [(_ name:expr) 
     #'(Bad the-Nothing name #f #f)]
    [(_ name:expr #:cause-result result:expr)
     #'(Bad the-Nothing name #f result)]
    [(_ #:invariant-of-data bare-v:expr #:broken-by op:id)
     #'(Bad bare-v 'broken-DI #'op #f)]
    [(_ #:bad-arg arg:id #:for op:id)
     #'(Bad the-Nothing 'bad-arg #'op arg)]
    [(_ #:exception obj:expr #:from op:id)
     #'(Bad the-Nothing (GotException-alert obj) #'op #f)]))

(define (monadic-not v)
  (cond-or-fail
   ((Good? v) (Good (not (Good-v v))))
   ((Bad? v) (make-Bad #:bad-arg v #:for monadic-not))))

;; We use this internally (only), to avoid (my-app not v).
(define-my-syntax my-not monadic-not not)

(define-syntax (monadic-if stx)
  (syntax-parse stx
    ((_ c:expr t:expr e:expr)
     #'(let ([v c])
         (if (Bad? v)
             (make-Bad #:bad-arg v #:for monadic-if)
             (if (Good-v v) t e))))))

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
    (with-syntax ([n n-stx]
                  [modifs #`'#,modifs-lst]
                  [params params-stx]
                  [(rec ...) (map mk-rec-stx alerts-lst)])
      #'(begin-for-syntax
          (free-id-table-set! 
           fun-meta-table 
           #'n 
           (AlertingFunction modifs #'params (list rec ...)))))))

;; An identifier that may be used to refer to a function's return
;; value in a post-condition alert expression context.
(define-syntax-parameter* value
  (syntax-rules ()))

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
    [(_ (n:id p:id ...) #:handler opts:maybe-alerts)
     (mk-reg-AlertingFunction 
      #'n '(handler) #'(p ...) (attribute opts.alerts))]
    [(_ (n:id p:id ...) opts:maybe-alerts)
     (mk-reg-AlertingFunction 
      #'n '(primitive) #'(p ...) (attribute opts.alerts))]))

;; For debugging only. Prints out function metadata.
(define-syntax* (declarations-for stx)
  (syntax-parse stx
    [(_ n:id ...)
     (for ([n-stx (syntax->list #'(n ...))])
       (write `(,n-stx ,(free-id-table-ref 
                         fun-meta-table n-stx
                         (lambda () 'undeclared))))
       (newline))
     #'(begin)]))

(define (maybe-get-pre-failure f-stx name v)
  (cond
   ((Bad? v) (Bad the-Nothing 'precond-check-fail f-stx v))
   (else
    (define gv (Good-v v))
    (unless (boolean? gv)
      (error 'maybe-get-pre-failure "expected boolean, got ~s" gv))
    (and gv (Bad the-Nothing name f-stx #f)))))

(define (maybe-get-post-failure f-stx r name v)
  (cond
   ((Bad? v) (Bad the-Nothing 'postcond-check-fail f-stx v))
   (else
    (define gv (Good-v v))
    (unless (boolean? gv)
      (error 'maybe-get-post-failure "expected boolean, got ~s" gv))
    (and gv (Bad (Good-v r) name f-stx #f)))))

(define-for-syntax (mk-UndeclaredFunction-app stx f-stx args-stx)
  (define arg-lst (syntax->list args-stx))
  (define param-lst (generate-temporaries arg-lst))
  (with-syntax ([f f-stx]
                [(a ...) arg-lst]
                [(p ...) param-lst])
    #'(let ([p a] ...)
        (cond 
         [(Bad? p)
          (make-Bad #:bad-arg p #:for f)] ...
         [else
          (let ([r (#%plain-app f (Good-v p) ...)])
            (cond
             ((data-invariant? r)
              (Good r))
             (else
              (make-Bad #:invariant-of-data r #:broken-by f))))]))))
  
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

  (define post-checked-r-stx
    (if (null? post-lst)
        #'r
        (with-syntax ([(post-check ...) 
                       (map
                        (lambda (post) 
                          #`(maybe-get-post-failure
                             #'#,f-stx r
                             '#,(AlertSpec-alert-name post)
                             #,(PostCond-cond-expr post)))
                        post-lst)])
          #'(syntax-parameterize ([value
                                   (make-rename-transformer #'r)])
              (cond
               [post-check => (lambda (x) x)] ...
               [else r])))))
  
  (with-syntax ([f f-stx]
                [(a ...) arg-lst]
                [(p ...) param-lst]
                [(pre-check ...) 
                 (map
                  (lambda (pre) 
                    #`(maybe-get-pre-failure
                       #'#,f-stx
                       '#,(AlertSpec-alert-name pre)
                       #,(PreCond-cond-expr pre)))
                  pre-lst)]
                [(exc-clause ...)
                 (map
                  (lambda (x) 
                    #`[#,(OnThrow-predicate-expr x)
                       (lambda (exn) 
                         (GotException '#,(AlertSpec-alert-name x)))])
                  throw-lst)]
                [post-checked-r post-checked-r-stx])
    (cond-or-fail
     ((memq 'primitive modifs)
      #'(let ([p a] ...)
          (cond 
           [pre-check => (lambda (x) x)] ...
           [(Bad? p)
            (make-Bad #:bad-arg p #:for f)] ...
           [else
            (let ([r (with-handlers (exc-clause ...)
                       (#%plain-app f (Good-v p) ...))])
              (cond
               ((GotException? r)
                (make-Bad #:exception r #:from f))
               ((not (data-invariant? r))
                (make-Bad #:invariant-of-data r #:broken-by f))
               (else
                (let ((r (Good r)))
                  post-checked-r))))])))
     ((memq 'regular modifs)
      #'(let ([p a] ...)
          (cond 
           [pre-check => (lambda (x) x)] ...
           [(Bad? p)
            (make-Bad #:bad-arg p #:for f)] ...
           [else
            (let ([r (with-handlers (exc-clause ...)
                       (#%plain-app f p ...))])
              (cond
               ((GotException? r)
                (make-Bad #:exception r #:from f))
               ((Bad? r)
                r)
               (else
                (define v (Good-v r))
                (if (data-invariant? v)
                    post-checked-r
                    (make-Bad #:invariant-of-data r #:broken-by f)))))])))
     ((memq 'handler modifs)
      #'(let ([p a] ...)
          (cond 
           [pre-check => (lambda (x) x)] ...
           [else
            (let ([r (with-handlers (exc-clause ...)
                       (#%plain-app f p ...))])
              (cond
               ((GotException? r)
                (make-Bad #:exception r #:from f))
               ((Bad? r)
                r)
               (else
                (define v (Good-v r))
                (if (data-invariant? v)
                    post-checked-r
                    (make-Bad #:invariant-of-data v #:broken-by f)))))]))))))

(define-syntax (monadic-app stx)
  (syntax-parse stx
    [(_ f:id a:expr ...)
     (define f-stx #'f)
     (define info (free-id-table-ref fun-meta-table f-stx #f))
     (cond-or-fail
      ((not info)
       (mk-UndeclaredFunction-app stx f-stx #'(a ...)))
      ((DirectFunction? info)
       #'(#%app f a ...))
      ((AlertingFunction? info)
       (mk-AlertingFunction-app info stx f-stx #'(a ...))))]))

(define-my-syntax my-app monadic-app #%app)

;; The opposite of a `do` in Haskell. The `b ...` expressions deal in
;; bare values. The "free variables" and their values should be given
;; as `[p e] ...` for unwrapping.
(define-syntax* (anti-do stx)
  (syntax-parse stx
    [(_ ([p:id e:expr] ...) b:expr ...+)
     #'(let ([p e] ...)
         (cond
          [(Bad? p)
           (make-Bad #:bad-arg p #:for anti-do)] ...
          [else
           (let ([p (Good-v p)] ...)
             (let ([r (begin-direct b ...)])
               (if (data-invariant? r)
                   (Good r)
                   (make-Bad #:invariant-of-data r 
                             #:broken-by anti-do))))]))]))

(define-syntax* (try stx)
  (define-syntax-class catch 
    #:description "#:catch clause for a `try`"
    #:attributes (info)
    (pattern
     ((a:id ...) h:expr ...+)
     #:attr info (cons (syntax->list #'(a ...)) #'(begin h ...))))
  
  (define-syntax-class catch-all
    #:description "#:catch clause for a `try`"
    #:attributes (then)
    (pattern
     ((~datum _) h:expr ...+)
     #:attr then #'(begin h ...)))
  
  (define (mk-clause id-lst then-stx r-stx)
    (with-syntax ([r r-stx]
                  [(a ...) id-lst]
                  [then then-stx])
      #'((and (Bad? r)
              (let ([n (Bad-origin-name r)])
                (or (eq? n (quote a)) ...)))
         then)))
  
  (define (mk-else-clause then-stx r-stx)
    (with-syntax ([r r-stx]
                  [then then-stx])
      #'((Bad? r)
         then)))
  
  (syntax-parse stx
    [(_ b:expr ...+ #:catch r:id cc:catch ... (~optional ec:catch-all))
     (define clause-stx-lst
       (append
        (for/list ([info (attribute cc.info)])
          (mk-clause (car info) (cdr info) #'r))
        (let ([x (attribute ec.then)])
          (if x (list (mk-else-clause x #'r)) '()))))
     (with-syntax ([(c ...) clause-stx-lst])
       #'(let ([r (begin b ...)])
           (cond
            c ...
            [else r])))]))

;; Handles any error in the `try-e` expression by evaluating the
;; `fail-e` expression instead. Equivalent to (try try-e #:catch ex [_
;; fail-e]).
(define-syntax* (default stx)
  (syntax-parse stx
    [(_ try-e fail-e)
     #'(let ([v try-e])
         (if (Bad? v)
             fail-e
             v))]))