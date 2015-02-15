#lang erda/rvm

#|

Some test code written in the erda/rvm language.

|#

(require (for-racket [racket/base define #%datum #%app displayln]))

(let-racket-selected
 (define x 1)
 (displayln x))

(let-racket-base
 (define x 2)
 (displayln x))

3

(bare . 4)

(if 1 2 3)

(if #f 7 8)

(require (only-in racket/base = < > + - * /))

(not (not #f))
(not #f)
(+ 1 2)
(+ (- 1 2) 3)

(define y 7)
(+ y 3)

(declare (/ x y) 
  #:alert ([div-by-0 pre-when (= y 0)]))

;(declarations-for x y /)

(/ 8 2)
(/ 9 3)
(/ 1 0)
(/ 1 (/ 1 0))

(require (only-in racket/base [define r.define]))
(r.define (f-failing-pre x) x)
(declare (f-failing-pre x)
  #:alert ([my-bad pre-unless (= (/ x 0) (/ x 0))]))
(f-failing-pre 7)

(define (f-seldom-happy x)
  #:alert ([not-happy pre-unless (= 1 x)])
  x)
(f-seldom-happy -1)
(f-seldom-happy 0)
(f-seldom-happy 1)
(f-seldom-happy (f-seldom-happy 1))
(f-seldom-happy (f-seldom-happy (f-seldom-happy 8)))

(require (only-in racket/base exn:fail?))
(declare (exn:fail? x) #:direct)

(define (raising-star x)
  #:alert ([threw-something on-throw exn:fail?])
  (if x 
      (let-racket-base (error 'raising-star)) 
      x))

(raising-star #f)
(raising-star #t)

(define (identity-likes-0 x)
  #:alert ([too-small pre-when (< x 0)]
           [too-large post-when (> value 0)])
  x)

(identity-likes-0 -5)
(identity-likes-0 5)
(identity-likes-0 0)

(define (handler-wants-good x) #:handler
  #:alert ([not-good pre-unless (good-result? x)])
  x)

(handler-wants-good 5)
(handler-wants-good (/ 1 0))

(require (only-in racket/bool symbol=?))
'y
(symbol=? 'a 'b)
(symbol=? 'x 'x)
;;'(x) ;; not allowed

(alert-name? 4)
(alert-name? 'bad-thing)
(alert-name=? 'bad 'bad)
(alert-name=? 'bad 'worse)
(alert-name=? 4 5)

(result? 4)
(result? (/ 1 0))
(good-result? 4)
(good-result? (/ 1 0))
(result-has-value? 4)
(result-has-value? (identity-likes-0 -7))
(result-has-value? (identity-likes-0 7))
(value-of-result (identity-likes-0 7))
(value-of-result 8)
(value-of-result (/ 1 0))

'great
(bare . #f)
(raise 'bad)
(raise-with-value 'worse 8)
(raise-with-cause 'still-worse (raise-with-cause 'even-worse (raise-with-cause 'worse (raise-with-value 'bad 999))))

(default (raise 'bad) (+ 9 1))
(default-to-bad (raise-with-value 'worse 11))
(default-to-bad 12)

(begin-direct 5)
(begin-direct (if #t (- 7 1) (and 7 8)))

(let ((y 5)) (anti-do ((x y)) (* 2 x)))
(let ((x 5)) (anti-do ((x x)) x))
(anti-do ((x (/ 7 0))) (* x 2))

(try 1 #:catch ex)
(try 1 2 #:catch ex)
(try 1 3 #:catch ex [(bad worse horrible) 9])
(try 1 (/ 1 0) #:catch ex [_ 9])
(try (raise 'bad) #:catch ex [(worse horrible) 9])
(try (raise 'bad) #:catch ex [(bad worse horrible) 9])

;; should we match the original error, the most recent, or any of
;; them? -- currently matching original one, so this does match
(try (+ 1 (/ 2 0) 3) #:catch ex [(div-by-0) 4])

(+ 1 (default-to-bad (raise-with-value 'bad 5)))
(+ 2 (default-to-bad 3))

(anti-do ((x 2)) (* (* (* x x) (* x x)) (* (* x x) (* x x))))

(begin 1 (/ 1 0)) ;; => div-by-0[/]
(begin (/ 1 0) 1) ;; => (Good 1)
(try 1 (/ 1 0) #:catch ex [_ 9]) ;; => (Good 9)
(try (/ 1 0) 1 #:catch ex [_ 9]) ;; => (Good 1)

(define (identity x) x)

(identity (identity 88))

(define (factorial x)
  (cond
   ((< x 0) (raise 'bad-arg))
   ((= x 0) 1)
   (else (* x (factorial (- x 1))))))

(factorial -4)
(factorial 0)
(factorial 6)
