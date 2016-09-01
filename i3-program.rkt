#lang erda/ga

#|
|#

(require (only-in racket/base writeln displayln))
(require (prefix-in rkt. racket/base))
(require racket/flonum)

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
