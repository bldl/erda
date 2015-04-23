#lang scribble/manual
@;;;###action ((master-file . "manual.scrbl") (quiet . t))
@(require scribble/eval "util.rkt"
	  (for-label erda/cxx))

@(define the-eval (make-base-eval))
@(the-eval '(require erda/cxx))

@title{@ErdaCxx}

@defmodulelang[erda/cxx]

The @deftech{@ErdaCxx} language is a statically typed language such that it includes an alerts mechanism for declarative error reporting, and transparently propagates errors as data values.

@ErdaCxx is very similar to @ErdaRkt, but with some notable differences:
@itemlist[

 @item{Provided that any referenced functions are implemented both for Racket and C++, the definitions appearing in a @racketmodname[erda/cxx] module (or collection thereof) may both be used directly from a Racket program, and translated into a C++ API and implementation usable from C++ programs. In contrast, the definitions appearing in a @racketmodname[erda/rvm] module are only intended for evaluation in the Racket VM.}

  @item{While the @ErdaCxx and @ErdaRkt implementations are largely based on the same code, the former does somewhat more work at compile time. This is to keep the runtime requirements smaller, and thus facilitate translation into C++. The only C++ runtime requirements for the @racketmodname[erda/cxx] language itself are a small subset of the C and C++11 standard libraries, and the @filepath{erda.hpp} header file.}
  
 @item{@ErdaCxx's functions and variables are typed, whereas in @ErdaRkt it is values that are typed. While the static types need not always be declared, the types of a program must be fully resolvable statically. For this purpose, the compiler features Hindley-Milner style type inference.}

 @item{For declaring types and other details relating to translating @ErdaCxx into C++, the language features support for various annotations (e.g., @racket[type], @racket[foreign], etc.) that may be specified for declarations; there are no such annotations in @|ErdaRkt|.}

 @item{Not everything from @ErdaRkt has been brought over to @ErdaCxx; notably, some of the error recovery supporting forms are missing, as is most of the runtime library. The focus in @ErdaCxx has been to include only the essentials in the language, and exclude more experimental features (such as @racket/ErdaRkt[try] and @racket/ErdaRkt[on-alert]). The idea is to improve and validate the design of these features in @ErdaRkt first, before bringing them into other Erda variants.}

]

This document describes the syntax and semantics a selection of those @|ErdaCxx| constructs that have notable differences to @ErdaRkt's. Overall, @ErdaCxx's syntactic constructs generally have the same semantics as in @ErdaRkt, and we do not document them separately here.

@defform*[((define #:type id maybe-annos)
           (define id maybe-annos expr)
	   (define (id arg ...) maybe-annos maybe-alerts expr ...+)
	   (define (id arg ...) #:handler maybe-annos maybe-alerts expr ...+)
	   (define (id arg ...) #:direct maybe-annos expr ...+))]{
Forms used to define types, variables and functions.

These forms have the same semantics as for @ErdaRkt-racket[define], with two notable exceptions. Firstly, there is a @racket[define] @racket[#:type] form, which is the same as for @Magnolisp-racket[define]. Second, all the @racket[define] variants accept optional annotations. The grammar for @racket[maybe-annos] is as described in @secref["Annotations" #:doc '(lib "magnolisp/manual.scrbl")].}

@defform*[((declare #:type id maybe-annos)
           (declare (id arg ...) maybe-annos maybe-alerts)
	   (declare (id arg ...) #:direct maybe-annos))]{
Forms used to specify information about types and functions, not to implement them, or to bind the identifier @racket[id]. The binding must already exist.

See @racket[define] for a description of the two notable differences between @ErdaCxx's @racket[declare] compared to @ErdaRkt-racket[declare] and @Magnolisp-racket[declare], as these differences are the same for both @racket[define] and @racket[declare].}

@section{C++ Translation Advising Annotations}

Some of the @ErdaCxx defining forms support a subset of the @seclink["Annotations" #:doc '(lib "magnolisp/manual.scrbl")]{annotations} that appear in the Magnolisp language. The supported annotations are:
@racket[type], @racket[export], @racket[foreign], and @racket[literal]. The purpose of these annotations is to instruct @|ErdaCxx|-to-C++ translation. Refer to Magnolisp documentation for more details about them.

@(close-eval the-eval)
