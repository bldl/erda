#lang scribble/manual
@(require "util.rkt")

@title[#:tag "erda"]{Erda}

@author["Tero Hasu"]

@tech[#:key "ErdaRkt" ErdaRkt], @tech[#:key "ErdaRktAssign" ErdaRktAssign], @tech[#:key "ErdaCxx" ErdaCxx], and @tech[#:key "ErdaGa" ErdaGa] constitute a family of small programming languages and implementations for experimenting with error handling mechanisms. We use the unqualified name @deftech{Erda} to refer to the language family as a whole, or any one member of the family where the languages are all alike in relevant respects.

The ``concrete'' syntax of Erda resembles that of Racket (and Scheme).

@include-section["erda-rvm.scrbl"]
@include-section["erda-sigma-rvm.scrbl"]
@include-section["erda-cxx.scrbl"]
@include-section["erda-ga.scrbl"]

@section[#:tag "erda-examples"]{Example Code}

For sample @ErdaRkt code, see the @filepath{i1-program.rkt} file of the Erda implementation codebase. For @ErdaGa code, see @filepath{i3-program.rkt}. Those programs should evaluate as is within the Racket VM; see the @exec{racket} command of your Racket installation.

For sample @ErdaCxx programs, see the @filepath{test-*.rkt} files and @filepath{program-*} projects in the @filepath{tests} directory of the codebase.

Most of the provided sample @ErdaCxx programs will evaluate as is within the Racket VM. To instead translate said programs into C++, see the Magnolisp documentation, or look at the @filepath{Makefile}s in the @filepath{program-*} directories for example invocations of the @exec{mglc} command-line tool.

To run basic tests to verify that the Magnolisp compiler is available and working, you may run:
@commandline{make test}

@section[#:tag "erda-repo"]{Source Code}

The Erda source code repository is hosted at:
@nested[#:style 'inset]{@url{https://github.com/bldl/erda}}

@section[#:tag "erda-install"]{Installation}

The pre-requisites for installing the software are:
@itemlist[

 @item{@bold{Racket.} The primary implementation language of Erda. Version 6.3 (or higher) of Racket is required; a known-compatible version is 6.5, but versions 6.3--6.6 are all expected to work.}

 @item{@bold{Magnolisp.} A language and compiler serving as a basis for the implementation of @|ErdaCxx|. A known-compatible revision of Magnolisp is @|magnolisp-git-rev|.}

]

The software and the documentation can be built from source, or installed using the Racket package manager.
Racket is required for building and installation.

Once Racket and its tools have been installed, the Magnolisp and Erda packages can be installed with the @exec{raco} commands:
@nested[#:style 'inset]{
@verbatim{
raco pkg install @|magnolisp-pkg-url|
raco pkg install @|erda-pkg-url|
}}

The above commands should install the library, the command-line tool(s), and a HTML version of the manual.

@section[#:tag "erda-license"]{License}

Except where otherwise noted, the following license applies:

Copyright © 2014--2016 University of Bergen and the authors.

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
