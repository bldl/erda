#lang erda/rvm

#|

Some test code written in the erda/rvm language.

|#

(require (for-racket [racket/base define #%datum #%app displayln]))
(require (only-in racket/base = < > + - * / add1 sub1
                  exn:fail:contract:divide-by-zero? list
                  [/ throwing-/]))
(require (submod magnolisp/util/print export-always))

(define (never-happy a b c)
  #:alert ([unhappy post-when (= 0 0)])
  a)

(never-happy 1 2 3)

(define (f-1 c a b)
  (block 
   (#:let x b)
   (#:when c #:let x a)
   x))

(define (f-2 c a b)
  (if c a b))

(define a-bad-v (raise 'bad))

(f-1 1 22 3)
(f-2 1 22 3)
(f-1 #f 2 33)
(f-2 #f 2 33)
(f-1 a-bad-v 2 3)
(f-2 a-bad-v 2 3)

(if (raise 'bad) 1 2)

(block 1)
(block 1 2)
(block (#:let x 7) x)
(block (#:let x 1) (#:let x 2) x)
(block (#:let x #t) (#:when x #:let x 17) x)
(block (#:let x #f) (#:when x #:let x 18) x)
(block (#:let x (raise 'bad)) (#:when x #:let x 19) x)

(declare (throwing-/ x y)
  #:alert ([div-by-0 on-throw exn:fail:contract:divide-by-zero?]))

(throwing-/ 7 8)
(throwing-/ 7 0)

(on-alert ([(throwing-/) 0])
  (throwing-/ 9 0))

(define (f1) 
  (raise 'f1-error))

(define (f2) 
  (raise 'f2-error))

(on-alert ([(f1) 7]) (f1))
(on-alert () 5 6)
(on-alert () 5 (raise-with-value 'bad 6))

(on-alert ([(f1) 'outer-f1])
  (on-alert ([(f2) 'only-f2])
    (writeln (list (f1) (f2)))
    (on-alert ([(f1) 'inner-f1])
      (writeln (list (f1) (f2))))))

(define (anything-but-Good-five? x) #:handler
  (not (and (good-result? x) (= x 5))))

(anything-but-Good-five? 0)
(anything-but-Good-five? 5)
(anything-but-Good-five? (raise 'bad))

(define (Bad-five? x) #:handler
  (and (bad-result? x) (result-has-value? x)
       (= (value-of-result x) 5)))

(Bad-five? 7)
(Bad-five? 5)
(Bad-five? (raise 'bad))
(Bad-five? (raise-with-value 'bad 5))

88
(result-has-value? 88)
(result-has-value? (raise-with-value 'bad 89))
(result-has-value? (raise 'bad))

(define (forced-add1 x) #:handler
  #:alert ([no-good pre-unless (or (good-result? x)
                                   (and (result-has-value? x)
                                        (good-result?
                                         (value-of-result x))))]
           [pre-five pre-when (Bad-five? x)]
           [post-five post-when (Bad-five? value)])
  (if (good-result? x) 
      (add1 x)
      (set-bad-result-value x (add1 (value-of-result x)))))

(forced-add1 77)
(forced-add1 5)
(forced-add1 (raise 'bad))
(forced-add1 (raise-with-value 'bad 90))
(forced-add1 (raise-with-value 'bad 5))
(forced-add1 (raise-with-value 'bad 4))

(+ 7 3)
(+ (raise 'bad) 3)
(+ 7 (raise 'bad))

(anti-do () 17)
(anti-do ((x 1)) 18)
(anti-do ((x (raise 'ugly))) 1 2)
(anti-do ((x 1) (y 2)) (+ x y))
(anti-do ((x 1) (y (raise 'horrible))) (+ x y))
(anti-do ((x (raise 'great)) (y 2)) (+ x y))

(define (+-equal x y)
  #:alert ([not-equal pre-unless (= x y)])
  (+ x y))

(+-equal 7 7)
(+-equal (raise 'bad) 7)
(+-equal 7 (raise 'bad))
(+-equal 7 8)

(declare (/ x y) 
  #:alert ([div-by-0 pre-when (= y 0)]))

(/ 27 7)
(/ (raise 'bad) 7)
(/ 27 (raise 'bad))
(/ 27 0)

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

(not (not #f))
(not #f)
(+ 1 2)
(+ (- 1 2) 3)

(define y 7)
(+ y 3)

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

(let ((y 5)) (anti-do ((x y)) (* 2 x)))
(let ((x 5)) (anti-do ((x x)) x))
(anti-do ((x (/ 7 0))) (* x 2))

(try 1 #:catch)
(try 1 #:catch [_ value])
(try 1 2 #:catch)
(try 1 3 #:catch [(bad worse horrible) 9])
(try (raise 'bad) #:catch [(worse horrible) 9])
(try (raise 'bad) #:catch [(bad worse horrible) 9])
(try (raise 'bad) 1 #:catch)
(try (raise 'bad) #:catch)
(try (raise 'bad) #:catch [(worse) 9])
(try (raise 'bad) #:catch [(worse) 9] [(bad) 10] [_ 11])
(try (raise 'horrible) #:catch [(worse) 9] [(bad) 10] [_ 11])
(try (raise 'horrible) #:catch [(worse) 9] [(bad) 10] [_ value])
(try (raise-with-value 'bad 10) #:catch [(worse) 9] [(bad) (default-to-bad value)] [_ 11])

;; should we match the original error, the most recent, or any of
;; them? -- currently matching original one, so this does match
(try (+ 1 (/ 2 0) 3) #:catch [(div-by-0) 4])

(+ 1 (default-to-bad (raise-with-value 'bad 5)))
(+ 2 (default-to-bad 3))

(anti-do ((x 2)) (* (* (* x x) (* x x)) (* (* x x) (* x x))))

(begin 1 (/ 1 0)) ;; => div-by-0[/]
(begin (/ 1 0) 1) ;; => (Good 1)
(try 1 (/ 1 0) #:catch [_ 9]) ;; => (Good 9)
(try (/ 1 0) 1 #:catch [_ 9]) ;; => (Good 1)

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

;; Cannot embed a Bad `bad-v` value like this since `raise-with-value`
;; cannot take it, as it is not a #:handler.
(raise-with-value 'bad-with-bad-value (raise 'another-bad))
