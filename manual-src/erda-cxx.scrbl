#lang scribble/manual
@(require scribble/eval "util.rkt"
	  (for-label erda/cxx))

@(define the-eval (make-base-eval))
@(the-eval '(require erda/cxx))

@title{@ErdaCxx}

@defmodulelang[erda/cxx]

The @deftech{@ErdaCxx} language is a statically typed language such that it includes an alerts mechanism for declarative error reporting, and transparently propagates errors as data values.

@ErdaCxx is very similar to @ErdaRkt, but with some notable differences:
@itemlist[

 @item{Provided that any referenced functions are implemented both for Racket and C++, the definitions appearing in a @racketmodname[erda/cxx] module (or collection thereof) may both be used directly from a Racket program, and translated into a C++ API and implementation usable from C++ programs. The definitions appearing in a @racketmodname[erda/rvm] module are only intended for evaluation in the Racket VM.}

  @item{While the @ErdaCxx and @ErdaRkt implementations are largely based on the same code, the former does somewhat more work at compile time. This is to keep the runtime requirements smaller, and thus facilitate translation into C++. The only C++ runtime requirements for the @racketmodname[erda/cxx] language itself are a small subset of the C and C++11 standard libraries, and the @filepath{erda.hpp} header file.}
  
 @item{@ErdaCxx's functions and variables are typed, whereas in @ErdaRkt it is values that are typed. While the static types need not always be declared, the types of a program must be fully resolvable statically. For this purpose, the compiler features Hindley-Milner style type inference.}

 @item{For declaring types and other details relating to translating @ErdaCxx into C++, the language features support for various annotations (e.g., @racket[type], @racket[foreign], etc.) that may be specified for declarations. There are no such annotations in @|ErdaRkt|.}

]

This document describes the syntax and semantics a selection of the @|ErdaCxx| constructs.

@; define
@; declare

@(close-eval the-eval)
