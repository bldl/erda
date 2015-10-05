#lang erda/rvm

(require "instr-file-erda.rkt"
         (prefix-in rkt. racket/base))

(provide main)

(define (main)
  (open-input-file "nxist"))
