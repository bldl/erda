#lang erda/ga

#|

A dummy robot comms client library, simulating a flaky Bluetooth
connection to the robot, with frequent `ConnectionFailed` alerts.

|#

(require (only-in racket/base
                  [#%app rapp]
                  =
                  set! quasiquote unquote
                  sleep random
                  display displayln newline
                  write writeln flush-output)
         erda/i3-internal)

(provide reconnect
         set-stabilization random-stabilization
         set-rgb-led random-rgb
         set-back-led random-color
         set-heading random-heading)

(define connected? #f)
(define stabilization? #f)

(define (print-state)
  (display "connected: ") (write connected?)
  (display " stabilization: ") (write stabilization?)
  (newline))

(define (comms-succeed?)
  (when (and connected? (= (random 5) 0))
    (set! connected? #f))
  ;;(rapp writeln `(comms-succeed? : ,(rapp Good-v connected?)))
  connected?)

(define (fail-randomly?)
  (= (random 25) 0))

(define (reconnect)
  (define t (random 5))
  (display "reconnecting...") (flush-output)
  (sleep t)
  (displayln "connected.")
  (set! connected? #t)
  (print-state)
  t)

(define (send-recv cmd v)
  #:alert ([ConnectionFailed pre-unless (comms-succeed?)]
           [RandomFailure pre-when (fail-randomly?)])
  ;;(sleep 1)
  (rapp displayln `(send-recv ,(rapp Good-v cmd) ,(rapp Good-v v)))
  v)

;; Set Stabilization (on/off value).
(define (set-stabilization v)
  (try
   (send-recv 'set-stabilization v)
   #:catch
   #:else
   (set! stabilization? v)
   (print-state)
   value))

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
  #:alert ([StabilizationOff pre-unless stabilization?])
  (send-recv 'set-heading v))

(define (random-heading)
  (random 360))
