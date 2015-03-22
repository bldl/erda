#lang racket/base

#|

This is a variant of erda/rvm such that it exports `set!`, and a
modified `if` with optional cleanup actions. There are no other
conditional forms in this language.

|#

(module reader syntax/module-reader 
  erda/sigma-rvm)

(module impl racket/base
  (require "i1-internal.rkt"
           (for-syntax racket/base syntax/parse))
  
  (provide my-if my-when my-unless)

  (define-syntax (monadic-if/cleanup stx)
    (syntax-parse stx
      [(_ c:expr t:expr e:expr (~optional (~seq #:cleanup cact:expr ...)))
       (with-syntax ([(act ...) (if (attribute cact)
                                    (syntax->list #'(cact ...))
                                    null)])
         #'(let ([v c])
             (if (Bad? v)
                 (begin0
                     (bad-condition #:bad-arg v #'monadic-if/cleanup)
                   act ...)
                 (if (Good-v v) t e))))]))

  (define-syntax (my-when stx)
    (syntax-parse stx
      [(_ c:expr ts:expr ...+ #:cleanup cact:expr ...)
       #'(my-if c (begin ts ...) (void) #:cleanup cact ...)]
      [(_ c:expr ts:expr ...+)
       #'(my-if c (begin ts ...) (void))]
      ))

  (define-syntax (my-unless stx)
    (syntax-parse stx
      [(_ c:expr ts:expr ...+ #:cleanup cact:expr ...)
       #'(my-if c (void) (begin ts ...) #:cleanup cact ...)]
      [(_ c:expr ts:expr ...+)
       #'(my-if c (void) (begin ts ...))]
      ))

  (define-my-syntax my-if monadic-if/cleanup if))

(require "i1-lang.rkt" "i1-lib.rkt" (submod "." impl))
(provide (except-out (all-from-out "i1-lang.rkt") 
                     if if-not or and cond block)
         (all-from-out "i1-lib.rkt")
         (rename-out [my-if if] [my-when when] [my-unless unless])
         set! #%top-interaction)
