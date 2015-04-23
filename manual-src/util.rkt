#lang at-exp racket

#|
|#

(require scribble/manual "../util.rkt")

@(define* ErdaRkt @elem{Erda@subscript{@italic{RVM}}})
@(define* ErdaRktAssign @elem{Erda@subscript{@italic{RVM}}@superscript{σ}})
@(define* ErdaCxx @elem{Erda@subscript{@italic{C++}}})

;;; 
;;; Racket's syntax
;;; 

(module m1 racket
  (require scribble/manual (for-label racket/base))
  (provide racket/Racket)
  (define-syntax (racket/Racket stx)
    (syntax-case stx ()
      [(_ datum)
       #`(racket #,(datum->syntax #'here (syntax->datum #'datum)))])))

(require (submod "." m1))
(provide racket/Racket)

(define-syntax-rule* (Racket-racket datum)
  @elem{Racket's @racket/Racket[datum]})

;;; 
;;; Erda_RVM's syntax
;;; 

(module m2 racket
  (require scribble/manual (for-label erda/rvm))
  (provide racket/ErdaRkt)
  (define-syntax (racket/ErdaRkt stx)
    (syntax-case stx ()
      [(_ datum)
       #`(racket #,(datum->syntax #'here (syntax->datum #'datum)))])))

(require (submod "." m2))
(provide racket/ErdaRkt)

(define-syntax-rule* (ErdaRkt-racket datum)
  @elem{@ErdaRkt's @racket/ErdaRkt[datum]})
