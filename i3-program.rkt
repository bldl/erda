#lang erda/ga

#|
|#

(require (only-in racket/base writeln displayln))
(require racket/flonum)

1
(thunk 2)
((thunk 3))

(if 1 6 7)
(if fl+ 9 9)
(if #f fl+ 9)

(anti-do () (fl- (fl+ 1.0 2.0) 0.5))
(anti-do ([a 1.0] [b 2.0])
  (fl/ (fl+ a b) 2.0))
