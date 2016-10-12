#lang erda/ga

#|

A dummy client library to a social networking web service.

|#

(provide create-account)

(require (only-in racket/base random =))

;; Returns a list of all contacts in the contact database. Requires
;; `READ_CONTACTS` (Android) permission.
(define (read-all-contacts)
  #:alert ([NoPermission pre-when #t])
  #f)

;; Uploads contact `lst`. Registers a new account with `data`, and
;; returns an accound ID.
(define (create-account+upload data lst)
  #:alert ([bad-arg pre-unless (list? lst)]
           [ConnectionFailed pre-when (= (random 5) 0)]
           [BadAccountData post-when (= value 0)])
  (random 10000))

(define (create-account data)
  (define lst (read-all-contacts))
  (create-account+upload data lst))
