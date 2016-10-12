#lang erda/ga

#|

An example of recovering from a failure when there are multiple
different choices for commands that may have failed, and we want to
retry until success, without our own bookkeeping.

|#

(require "lib-robot-1.rkt"
         (only-in racket/base [#%app rapp]
                  = random
                  sleep display write writeln newline flush-output))

(define (user-choice)
  (display "user>") (flush-output)
  (sleep (random 4))
  (define choice (random 4))
  (write choice) (newline)
  choice)

;; Recovers `r`, or returns a bad result.
(define (recover r) #:handler
  (when (bad-result? r)
    (display "recovering: ")
    (rapp writeln r))
  (try r
    #:catch
    [(ConnectionFailed)
     (do (reconnect)
         (recover (redo r)))]
    [(StabilizationOff)
     (do (recover (set-stabilization #t))
         (recover (redo r)))]))

(define (main)
  (define choice (user-choice))
  (define reply
    (cond
      [(= choice 0) (set-stabilization (random-stabilization))]
      [(= choice 1) (set-rgb-led (random-rgb))]
      [(= choice 2) (set-back-led (random-color))]
      [#:else (set-heading (random-heading))]))
  (do (recover reply)
      (main)))

(main)
