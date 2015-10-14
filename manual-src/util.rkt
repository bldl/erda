#lang at-exp racket

#|
|#

(require scribble/manual "../util.rkt")
(require racket/runtime-path)

(define* ErdaRkt @elem{Erda@subscript{@italic{RVM}}})
(define* ErdaRktAssign @elem{Erda@subscript{@italic{RVM}}@superscript{σ}})
(define* ErdaCxx @elem{Erda@subscript{@italic{C++}}})

(define-runtime-path MAGNOLISPGITREV-file
  "../MAGNOLISPGITREV")

(define* MAGNOLISPGITREV
  (string-trim
   (call-with-input-file
     MAGNOLISPGITREV-file
     port->string)))

(define* short-MAGNOLISPGITREV
  (list->string
   (for/list ((ch (in-string MAGNOLISPGITREV 0 7)))
     ch)))

;;; 
;;; Racket syntax
;;; 

(module Racket-m racket
  (require scribble/manual (for-label racket/base))
  (provide racket/Racket)
  (define-syntax (racket/Racket stx)
    (syntax-case stx ()
      [(_ datum)
       #`(racket #,(datum->syntax #'here (syntax->datum #'datum)))])))

(require (submod "." Racket-m))
(provide racket/Racket)

(define-syntax-rule* (Racket-racket datum)
  @elem{Racket's @racket/Racket[datum]})

;;; 
;;; Magnolisp syntax
;;; 

(module Magnolisp-m racket
  (require scribble/manual (for-label (only-meta-in 0 magnolisp)))
  (provide racket/Magnolisp)
  (define-syntax (racket/Magnolisp stx)
    (syntax-case stx ()
      [(_ datum)
       #`(racket #,(datum->syntax #'here (syntax->datum #'datum)))])))

(require (submod "." Magnolisp-m))
(provide racket/Magnolisp)

(define-syntax-rule* (Magnolisp-racket datum)
  @elem{Magnolisp's @racket/Magnolisp[datum]})

;;; 
;;; Erda_RVM syntax
;;; 

(module ErdaRkt-m racket
  (require scribble/manual (for-label erda/rvm))
  (provide racket/ErdaRkt)
  (define-syntax (racket/ErdaRkt stx)
    (syntax-case stx ()
      [(_ datum)
       #`(racket #,(datum->syntax #'here (syntax->datum #'datum)))])))

(require (submod "." ErdaRkt-m))
(provide racket/ErdaRkt)

(define-syntax-rule* (ErdaRkt-racket datum)
  @elem{@ErdaRkt's @racket/ErdaRkt[datum]})
