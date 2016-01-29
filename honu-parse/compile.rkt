#lang racket/base

(require (for-syntax racket/base
                     "debug.rkt")
         (for-template racket/base)
         "debug.rkt")

;; to get syntax as a literal

(provide (all-defined-out))

(define parsed-property 'honu-parsed)

(define (parsed-syntax syntax)
  (debug "Add parsed syntax property to ~a\n" (syntax->datum syntax))
  (if syntax
    (syntax-property syntax parsed-property #t)
    syntax))

(define (parsed-syntax? syntax)
  (if syntax
    (syntax-property syntax parsed-property)
    syntax))

(define-syntax (racket-syntax stx)
  (syntax-case stx ()
    [(_ form)
     (begin
       (debug 2 "Racket syntax ~a\n" (syntax->datum #'form))
       #'(parsed-syntax #'form))]))
