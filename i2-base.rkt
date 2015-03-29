#lang racket/base

#|

Defines an implementation language for the `erda/cxx` standard library.

|#

(require "i2-internal.rkt")
(provide (rename-out [i2-module-begin #%module-begin]))

(require "i2-surface.rkt")
(provide (all-from-out "i2-surface.rkt"))
