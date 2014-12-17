#lang scribble/manual
@(require scribble/eval
	  (for-label erda/rvm))

@(define Erda-rkt @elem{Erda@subscript{@italic{RVM}}})
@(define Erda-cxx @elem{Erda@subscript{@italic{C++}}})

@(define the-eval (make-base-eval))
@(the-eval '(require erda/rvm))

@title{Erda}

@author["Tero Hasu"]

These are @deftech{@Erda-rkt} and @deftech{@Erda-cxx}, small programming languages and implementations for experimenting with error handling mechanisms.

@section{License}

Except where otherwise noted, the following license applies:

Copyright © 2012-2014 University of Bergen and the authors.

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

@(close-eval the-eval)
