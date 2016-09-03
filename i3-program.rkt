#lang erda/ga

#|
|#

(require (only-in racket/base
                  = < > + - * / not
                  exn:fail:contract:divide-by-zero?
                  writeln displayln))
(require (only-in racket/function identity))
(require (only-in racket/math nan?))
(require (prefix-in rkt. racket/base))
(require racket/flonum)

(raise 'bad)
(raise 'worse)

(define (my-/ x y)
  #:alert ([div-by-0 pre-when (= y 0)])
  (/ x y))

(my-/ 1 1)
(my-/ 0 1)
(my-/ 0 0)

(declare (catching-/ x y) #:is /
  #:alert ([div-by-0 on-throw exn:fail:contract:divide-by-zero?]))
(declare (pre-/ x y) #:is /
  #:alert ([div-by-0 pre-when (= y 0)]))
(declare (post-/ x y) #:is fl/
  #:alert ([div-by-0 post-when (nan? value)]))
(declare (bad-pre-/ x y) #:is /
  #:alert ([div-by-0 pre-unless (catching-/ x y)]))

(catching-/ 1 1)
(catching-/ 0 1)
(catching-/ 0 0)
(pre-/ 1 1)
(pre-/ 0 1)
(pre-/ 0 0)
(post-/ 1.0 1.0)
(post-/ 0.0 1.0)
(post-/ 0.0 0.0)
(bad-pre-/ 1 1)
(bad-pre-/ 0 1)
(bad-pre-/ 0 0)

(define (fourty-two x) #:handler
  #:alert ([not-bad pre-unless (bad-result? x)])
  42)

(fourty-two (pre-/ 0 1))
(fourty-two (pre-/ 0 0))

1
(thunk 2)
((thunk 3))

(if 1 6 7)
(if fl+ 9 9)
(if #f fl+ 9)

(>>= 7 (lambda (x) (rkt.add1 x)))
(do [x <- 7] (rkt.add1 x))
(do [x <- 7] [y <- (rkt.add1 x)] y)
(do 7 (rkt.add1 7))

(anti-do () (fl- (fl+ 1.0 2.0) 0.5))
(anti-do ([a 1.0] [b 2.0])
  (fl/ (fl+ a b) 2.0))

(define two 2)
(define (make-two) #:direct two)
two
(make-two)

(result? two)
(good-result? two)
(result-has-value? two)

(and 1 2)
(or 1 2)
(cond [#f 1] [#:else 2])

(declare (id x) #:is identity #:direct)
(id 7)
