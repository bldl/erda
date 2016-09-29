#lang at-exp racket

#|
|#

(require scribble/manual "../util.rkt")

(define* ErdaRkt @elem{Erda@subscript{@italic{RVM}}})
(define* ErdaRktAssign @elem{Erda@subscript{@italic{RVM}}@superscript{σ}})
(define* ErdaCxx @elem{Erda@subscript{@italic{C++}}})
(define* ErdaGa @elem{Erda@subscript{@italic{GA}}})

;;; 
;;; software revisions
;;; 

(require racket/runtime-path)

(define-runtime-path GITREVDIR "..")

(define (git-rev-file pkg)
  (build-path
   GITREVDIR
   (string-append (string-upcase pkg) "GITREV")))

(define* (git-rev pkg)
  (string-trim
   (call-with-input-file
     (git-rev-file pkg)
     port->string)))

(define* (maybe-git-rev pkg)
  (with-handlers ([exn:fail:filesystem?
                   (lambda (ex) #f)])
    (git-rev pkg)))

(define* (git-rev-shorten rev)
  (list->string
   (for/list ([ch (in-string rev 0 7)])
     ch)))

(define* (short-git-rev pkg)
  (git-rev-shorten (git-rev pkg)))

(define* (maybe-short-git-rev pkg)
  (define rev (maybe-git-rev pkg))
  (and rev (git-rev-shorten rev)))

(define* (sources-browse-url user pkg)
  (define rev (maybe-git-rev pkg))
  (if rev
      (format
       "https://github.com/~a/~a/tree/~a"
       user pkg rev)
      (format
       "https://github.com/~a/~a"
       user pkg)))

(define* (pkg-install-url user pkg)
  (define rev (maybe-short-git-rev pkg))
  (if rev
      (format "git://github.com/~a/~a#~a" user pkg rev)
      (format "git://github.com/~a/~a" user pkg)))

(define* magnolisp-git-rev
  (git-rev "magnolisp"))

(define* magnolisp-pkg-url
  (pkg-install-url "bldl" "magnolisp"))

(define* erda-pkg-url
  (pkg-install-url "bldl" "erda"))

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

(define-syntax-rule* (ErdaGa-like-ErdaRkt id form?)
  @elem{@ErdaGa's @defidentifier[#'id #:form? form?]
                  is like @ErdaRkt-racket[id]})
