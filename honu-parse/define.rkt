#lang racket/base
(require (for-syntax racket/base
                     syntax/parse
                     syntax/define
                     "compile.rkt"))

(provide (all-defined-out))

(define-syntax-rule (define-literal name ...)
  (begin
   (define-syntax name (lambda (stx)
                         (raise-syntax-error 'name
                                             "this is a literal and cannot be used outside a macro" (syntax->datum stx))))
   ...))

(define-syntax-rule (define-literal+set set literal ...)
                    (begin
                      (define-literal literal ...)
                      (begin-for-syntax
                        (define-literal-set set (literal ...)))))
