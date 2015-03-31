#lang racket/base

#|

|#

(require "i2-internal.rkt"
         "util.rkt" "util/provide-syntax.rkt"
         (rename-in magnolisp/surface 
                    [define mgl.define]
                    [declare mgl.declare])
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
         (rename-out [mgl.declare declare]) ;; xxx temporarily
         (rename-out [mgl.define define]) ;; xxx temporarily
         (from-prefixed-out my- quote 
                            ;;if if-not or and cond define ;; xxx
                            )
         value)

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
