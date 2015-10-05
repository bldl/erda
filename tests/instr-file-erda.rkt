#lang erda/rvm

#|

An Erda API with instrumented file access functions that simulate
random failures.

|#

(require "instr-file-racket.rkt"
         (only-in racket/base exn:fail?))

(provide open-input-file open-output-file copy-port
         close-input-port close-output-port)

(declare (open-input-file fn)
  #:alert ([io-error on-throw exn:fail?]))

(declare (open-output-file fn)
  #:alert ([io-error on-throw exn:fail?]))

(declare (copy-port in out)
  #:alert ([io-error on-throw exn:fail?]))
