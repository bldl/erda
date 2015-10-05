#lang racket

#|
|#

(require rackunit)

(define-syntax (this-source stx)
  (quasisyntax/loc stx (unsyntax (syntax-source stx))))

(define src-file-dir
  (let ((this-path (this-source)))
    (define-values (b n mbd)
      (split-path this-path))
    b))

(define (run-rvm-file fn)
  (check-not-exn
   (thunk
    (let ((main (dynamic-require `(file ,(path->string fn)) 'main)))
      (main)))
   (format "failed to evaluate program ~a" fn)))
    
(define (run-rvm-files)
  (for ((bn (directory-list src-file-dir))
        #:when (regexp-match-exact? #rx"test-rvm-.*[.]rkt" bn))
    (define fn (build-path src-file-dir bn))
    (run-rvm-file fn)))

(module* test #f
  (run-rvm-files))
