#lang racket/base

#|

A functional programming oriented Erda, that closely builds around
guarded algebra semantics.

|#

(module reader syntax/module-reader 
  erda/ga)

(require "i3-surface.rkt" "i3-lib.rkt")
(provide (all-from-out "i3-surface.rkt" "i3-lib.rkt"))

(provide #%top-interaction)
