#lang racket/base

#|

Defines a subset of the the `erda/rvm` language, one that has somewhat
fewer runtime requirements.

|#

(module reader syntax/module-reader 
  erda/cxx)

(require "i2-internal.rkt")
(provide (rename-out [i2-module-begin #%module-begin]))

(require "i2-surface.rkt")
(provide (all-from-out "i2-surface.rkt"))

(require "i2-lib.rkt")
(provide (all-from-out "i2-lib.rkt"))

(provide #%top-interaction)
