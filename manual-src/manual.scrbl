#lang scribble/manual
@(require "util.rkt")

@title{Erda}

@author["Tero Hasu"]

@ErdaRkt, @ErdaRktAssign, and @ErdaCxx constitute a family of small programming languages and implementations for experimenting with error handling mechanisms. We use the unqualified name @deftech{Erda} to refer to the language family as a whole, or any one member of the family where the languages are all alike in relevant respects.

The ``concrete'' syntax of Erda resembles that of Racket (and Scheme).

@include-section["erda-rvm.scrbl"]
@include-section["erda-sigma-rvm.scrbl"]
@include-section["erda-cxx.scrbl"]

@section{Example Code}

For sample @ErdaRkt code, see the @filepath{i1-prog.rkt} file of the Erda implementation codebase. Said code should evaluate as is within the Racket VM; see the @exec{racket} command of your Racket installation.

For sample @ErdaCxx programs, see the @filepath{test-*.rkt} files and @filepath{program-*} projects in the @filepath{tests} directory of the codebase.

Most of the provided sample @ErdaCxx programs will evaluate as is within the Racket VM. To instead translate said programs into C++, see the Magnolisp documentation, or look at the @filepath{Makefile}s in the @filepath{program-*} directories for example invocations of the @exec{mglc} command-line tool.

To run basic tests to verify that the Magnolisp compiler is available and working, you may run:
@commandline{make test}

@section{Source Code}

A Git repository of the Erda source code can be found at:
@nested[#:style 'inset]{@url{https://github.com/bldl/erda}}

@section[#:tag "install"]{Installation}

The pre-requisites for installing the software are:
@itemlist[

 @item{@bold{Racket.} The primary implementation language of Erda. Version 6 (or higher) of Racket is required; a known-compatible version is 6.2.1, but versions 6.1--6.2.1 are all expected to work.}

 @item{@bold{Magnolisp.} A language and compiler serving as a basis for the implementation of @|ErdaCxx|. A known-compatible revision of Magnolisp is @|MAGNOLISPGITREV|.}

]

The software and the documentation can be built from source, or installed using the Racket package manager.
Racket is required for building and installation.

Once Racket and its tools have been installed, the Magnolisp and Erda packages can be installed with the @exec{raco} commands:
@nested[#:style 'inset]{
@verbatim{
raco pkg install git://github.com/bldl/magnolisp#@|short-MAGNOLISPGITREV|
raco pkg install git://github.com/bldl/erda
}}

The above commands should install the library, the command-line tool(s), and a HTML version of the manual.

@section{License}

Except where otherwise noted, the following license applies:

Copyright © 2014-2015 University of Bergen and the authors.

Authors: Tero Hasu

Permission is hereby granted, free of charge, to any person
obtaining a copy of this software and associated documentation files
(the "Software"), to deal in the Software without restriction,
including without limitation the rights to use, copy, modify, merge,
publish, distribute, sublicense, and/or sell copies of the Software,
and to permit persons to whom the Software is furnished to do so,
subject to the following conditions:

The above copyright notice and this permission notice shall be
included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
