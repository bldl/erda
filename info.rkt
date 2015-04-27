#lang info
(define name "Erda")
(define blurb
  '("Programming languages for experimenting with error handling."))
(define scribblings '(("manual-src/manual.scrbl" () (experimental))))
(define compile-omit-paths '("dist" "retired" "tests"))
(define deps '("base" "magnolisp"))
