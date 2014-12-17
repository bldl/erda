#lang racket/base

#|
|#

(require racket/provide-syntax
         (for-syntax racket/base syntax/parse))

(provide from-prefixed-out)

(define-provide-syntax (from-prefixed-out stx)
  (syntax-parse stx
    [(_ pfx:id n:id ...+)
     (define pfx-s (symbol->string (syntax-e #'pfx)))
     (define (prefix stx)
       (define s (string-append pfx-s (symbol->string (syntax-e stx))))
       (datum->syntax stx (string->symbol s)))
     (with-syntax ([([y-pfx n-pfx] ...)
                    (for/list ([n-pfx (syntax->list #'(n ...))])
                      #`[#,(prefix n-pfx) #,n-pfx])])
       #'(rename-out [y-pfx n-pfx] ...))]))


