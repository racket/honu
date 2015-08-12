#lang racket/base

(require (for-syntax racket/base
                     syntax/parse
                     enforest
                     "literals.rkt"
                     "util.rkt")
         racket/class
         (only-in "honu.rkt"
                  honu-declaration
                  separate-ids)
         "literals.rkt"
         (only-in "operator.rkt"
                  honu-equal)
         enforest)

(begin-for-syntax
  (define (replace-with-public method)
    (syntax-parse method #:literals (define)
      [(define (name args ...) body ...)
       (racket-syntax (define/public (name args ...) body ...))]))
  (define-literal-set equals (honu-equal))
  (define-splicing-syntax-class honu-class-thing
                                #:literal-sets (equals)
    [pattern method:honu-function-pattern
             #:with result (replace-with-public (local-binding method.result))]
    [pattern var:honu-declaration
             #:with result #'(field [var.name var.expression] ...)]))

(provide honu-class)
(define-honu-syntax honu-class
  (lambda (code)
    (syntax-parse code #:literal-sets (cruft)
      ;; FIXME: empty parenthesis for constructor arguments should be optional
      [(_ name (#%parens (~var constructor-argument (separate-ids (literal-syntax-class honu-comma) (literal-syntax-class honu-comma))))
          (#%braces method:honu-class-thing ...) . rest)
       (define class
         (racket-syntax (define name (class* object% ()
                                         (super-new)
                                         (init-field constructor-argument.id ...)
                                         method.result ...))))
       (values class (local-binding rest) #t)])))

(provide honu-new)
(define-honu-syntax honu-new
  (lambda (code)
    (syntax-parse code #:literal-sets (cruft)
      [(_ name (#%parens arg:honu-expression/comma) . rest)
       (define new (racket-syntax (make-object name (let () arg.result) ...)))
       (values
         new
         (local-binding rest)
         #f)])))
