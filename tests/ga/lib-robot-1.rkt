#lang erda/ga

#|

A dummy robot comms client library, simulating a flaky Bluetooth
connection to the robot, with frequent `ConnectionFailed` alerts.

|#

(require (only-in racket/base
                  [#%app rapp] [list rlist]
                  =
                  set! quasiquote unquote
                  sleep random
                  display displayln newline
                  make-hasheq hash-set! hash-ref
                  write writeln flush-output)
         erda/i3-internal)

(provide reconnect
         set-stabilization random-stabilization
         set-rgb-led random-rgb
         set-back-led random-color
         set-heading random-heading)

(define st (make-hasheq))
(define (st-get n) (hash-ref st n #f))
(define (st-set! n v) (hash-set! st n v))

(define (connected?) (st-get 'connected))
(define (stabilized?) (st-get 'set-stabilization))

(define (print-state)
  (displayln st))

(define (comms-succeed?)
  (when (and (connected?) (= (random 5) 0))
    (st-set! 'connected #f)
    (print-state))
  (connected?))

(define (fail-randomly?)
  (= (random 25) 0))

(define (reconnect)
  (define t (random 5))
  (display "reconnecting...") (flush-output)
  (sleep t)
  (displayln "connected.")
  (st-set! 'connected #t)
  (print-state)
  t)

(define (send-recv cmd v)
  #:alert ([ConnectionFailed pre-unless (comms-succeed?)]
           [RandomFailure pre-when (fail-randomly?)])
  (st-set! cmd v)
  (displayln (rlist cmd v "OK"))
  (print-state)
  v)

;; Set Stabilization (on/off value).
(define (set-stabilization v)
  (send-recv 'set-stabilization v))

(define (random-stabilization)
  (if (= 0 (random 2)) #t #f))

;; Set RGB LED Output (uses normal RGB coding, but colour resolution
;; is not that good).
(define (set-rgb-led v)
  (send-recv 'set-rgb-led v))

(define (random-rgb)
  (list (random 265) (random 265) (random 265)))

;; Set Back LED Output (monochrome scale 0-255).
(define (set-back-led v)
  (send-recv 'set-back-led v))

(define (random-color)
  (random 265))

;; Set Heading (2 byte value representing 0-359 degrees). Requires
;; stabilization to be on.
(define (set-heading v)
  #:alert ([StabilizationOff pre-unless (stabilized?)])
  (send-recv 'set-heading v))

(define (random-heading)
  (random 360))
