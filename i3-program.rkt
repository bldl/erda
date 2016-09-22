#lang erda/ga

#|
|#

(require (only-in racket/base [#%app rapp]
                  = < > + - * / add1 sub1
                  exn:fail:contract:divide-by-zero? exit
                  writeln displayln))
(require (only-in racket/function identity))
(require (only-in racket/list first second third))
(require (only-in racket/math nan?))
(require (prefix-in rkt. racket/base))
(require racket/flonum)

(if #t 1 0)
(define bad-if (if (raise 'bad) 1 0))
bad-if
(redo bad-if)
(redo-app bad-if #t (thunk 1) (thunk 0))
(redo-apply bad-if (args-replace-first #t (bad-result-args bad-if)))
(bad-result-args bad-if)
(args-replace-first #t (bad-result-args bad-if))

(define (f1) 
  (raise 'f1-error))
(define (f2) 
  (raise 'f2-error))
(on-alert ([(f1) 7]) (f1)) ;; 7
(on-alert () 5 6) ;; 6
(on-alert () 5 (raise-with-value 'bad 6))
(on-alert ([(f1) 'outer-f1])
  (on-alert ([(f2) 'only-f2])
    (writeln (list (f1) (f2)))
    (on-alert ([(f1) 'inner-f1])
      (writeln (list (f1) (f2)))
      'done-on-alert)))

(define bad-0 (raise-with-value 'bad 0))
(>>= 7 (lambda (x) (rkt.add1 x)))
(do [x <- 7] (rkt.add1 x))
(do [x <- 7] [y <- (rkt.add1 x)] y)
(do 7 (rkt.add1 7))
(do bad-0 8) ;; bad
(do [x <- bad-0] (default-to-bad x)) ;; bad

(define b1 (car (list)))
b1
(bad-result-fun b1)
(bad-result-args b1)
  
(redo (car (list)))

(define bad+1 (add1 (raise 'bad)))
bad+1
(redo bad+1)

(let ((x (raise 'bad)))
  (redo x))
(let ((x (raise 'bad)))
  (redo-app x 'worse))

(args-list? (args-list 1 2))
(args-list? (args-list (raise 'bad)))
(apply add1 (args-list 1))
(apply + (args-list 1 2))

(let ((bad (if (raise 'bad) 1 0)))
  (args-replace-first #t (bad-result-args bad)))

(args-replace-first 1 2)
(args-replace-first (raise 'worse) (args-list (raise 'bad) 1 (raise 'fair)))

(list)
(list 1)
(list 1 2 3)
(list? (list 1 2))
(car (list))

(try 1 #:catch)
(try 1 #:catch [_ (add1 value)])
(try 1 2 #:catch)
(try 1 3 #:catch [(bad worse horrible) 9])
(try (raise 'bad) #:catch [(worse horrible) 9])
(try (raise 'bad) #:catch [(bad worse horrible) 9])
(try (raise 'bad) 1 #:catch)
(try (raise 'bad) #:catch)
(try (raise 'bad) #:catch [(worse) 9] [(bad) 10] [_ 11])
(try (raise 'horrible) #:catch [(worse) 9] [(bad) 10] [_ 11])
(try (raise 'horrible) #:catch [(worse) 9] [(bad) 10] [_ value])
(try (raise-with-value 'bad 10) #:catch [(worse) 9] [(bad) (default-to-bad value)] [_ 11])

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

(let-direct () (fl- (fl+ 1.0 2.0) 0.5))
(let-direct ([a 1.0] [b 2.0])
  (fl/ (fl+ a b) 2.0))

(define two 2)
(define (make-two) #:direct two)
two
(make-two)

(result? two)
(good-result? two)
(result-has-value? two)

(and 1 22)
(or 1 22)
(cond [#f 1] [#:else 22])

(declare (id x) #:is identity #:direct)
(id 7)
