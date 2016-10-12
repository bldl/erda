#lang erda/ga

#|

An example of recovering from a failure in a third-party library.

|#

(require "lib-social-1.rkt")

;; Some account details supposedly already interactively queried from
;; the user.
(define account-data
  #f)

;; Rewrites history recorded in value `v`.
(define (rewrite v) #:handler
  (if (bad-result? v)
      (if (alert-name=?
           (bad-result-alert-name v)
           'NoPermission)
          ;; Rewrite as a (Good '()).
          null
          ;; Rewrite all arguments recursively.
          (bad-result-args-map rewrite v))
      v))

;; This fails to deliver a fresh account ID, due to a NoPermission
;; error if nothing else.
(create-account account-data)

;; Here we get the ID unless the failure was due to a ConnectionFailed
;; error.
(try
 (create-account account-data)
 #:catch
 [(NoPermission)
  (redo (rewrite value))])

(define account-id
  (try
   (create-account account-data)
   #:catch
   [(NoPermission)
    (redo (rewrite value))]))

account-id
