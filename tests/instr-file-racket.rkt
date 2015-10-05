#lang racket/base

#|

A Racket API with instrumented file access functions that simulate
random failures.

|#

(require
 (prefix-in rkt.
            (combine-in
             (only-in racket/base
                      open-input-file
                      open-output-file
                      close-input-port
                      close-output-port)
             (only-in racket/port
                      copy-port))))

(provide open-input-file
         open-output-file
         copy-port
         close-input-port
         close-output-port)

(define (fail-randomly?)
  (= (random 5) 0))

(define (maybe-fail-randomly make-msg)
  (when (fail-randomly?)
    (raise
     (make-exn:fail:filesystem
      (make-msg)
      (current-continuation-marks)))))

(define (open-input-file fn)
  (maybe-fail-randomly
   (lambda () (format "error opening file ~s for reading: seek error" fn)))
  (define in (rkt.open-input-file fn #:mode 'binary))
  (displayln "input port opened")
  in)

(define (open-output-file fn)
  (maybe-fail-randomly
   (lambda () (format "error opening file ~s for writing: permission denied" fn)))
  (define out (rkt.open-output-file fn #:mode 'binary
                                    #:exists 'truncate/replace))
  (displayln "output port opened")
  out)

(define (copy-port in out)
  (maybe-fail-randomly
   (lambda () "IO error copying between ports"))
  (rkt.copy-port in out)
  (displayln "data transfer completed"))

(define (close-input-port in)
  (rkt.close-input-port in)
  (displayln "input port closed"))

(define (close-output-port out)
  (rkt.close-output-port out)
  (displayln "output port closed"))
