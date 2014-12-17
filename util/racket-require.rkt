#lang racket/base

#|

Support for bringing in Racket names into a local context. The names
to be included can be specified just once using (require (for-racket
...)).

|#

(require racket/require-syntax
         (for-syntax racket/base syntax/parse))

(provide let-racket-full let-racket-base
         for-racket let-racket-selected)

(define-syntax (define-let-local-require stx)
  (syntax-parse stx
    ((_ name:id mod:id)
     #'(define-syntax (name ctx)
         (syntax-parse ctx
           ((_ . es)
            (with-syntax ([path (datum->syntax ctx 'mod)])
              #'(let ()
                  (local-require path)
                  . es))))))))

(define-let-local-require let-racket-full racket)
(define-let-local-require let-racket-base racket/base)

(define-for-syntax racket-req-lst '())

;; "Requires" symbols `sym ...` from module `mod` into the scope of
;; any `let-racket-selected` form.
(define-require-syntax (for-racket stx)
  (define-syntax-class spec
    #:description "let-racket-selected require specification"
    #:attributes (mod sym-lst)
    [pattern (mod:id sym:id ...)
             #:attr sym-lst (syntax->list #'(sym ...))])
  
  (syntax-parse stx
    ((_ req:spec ...)
     (set! racket-req-lst 
           (append 
            (map cons (attribute req.mod) (attribute req.sym-lst))
            racket-req-lst))
     ;;(writeln racket-req-lst)
     #'(combine-in))))

;; Like (let () ...), but also does a `local-require` of selected
;; Racket names.
(define-syntax (let-racket-selected stx)
  (syntax-parse stx
    ((_ . es:expr)
     (with-syntax (((req-spec ...)
                    (map
                     (lambda (xs) 
                       #`(only-in 
                          ;; Necessary since we want to import not to
                          ;; top-level module context, but to the
                          ;; specific local context `stx`. This does
                          ;; not break hygiene for as long as the
                          ;; module spec is just a module name (and we
                          ;; do not allow any other form of module
                          ;; specification).
                          #,(datum->syntax stx (syntax-e (car xs)))
                          #,@(cdr xs)))
                     racket-req-lst)))
       #'(let ()
           (local-require req-spec ...)
           . es)))))
