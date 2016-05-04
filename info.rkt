#lang info
(define name "Erda")
(define blurb
  '("Programming languages for experimenting with error handling."))
(define scribblings '(("manual-src/erda.scrbl" () (experimental))))
(define compile-omit-paths '("dist" "retired" "tests"))
(define deps '(("base" #:version "6.3") "magnolisp" "scribble-lib"))
(define build-deps '("at-exp-lib" "racket-doc" "rackunit-lib"))
