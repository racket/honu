#lang racket/base
(require (for-syntax racket/base
                     syntax/parse
                     syntax/define
                     "compile.rkt"
                     "transformer.rkt"))

(provide (all-defined-out))

(define-syntax (define-honu-syntax stx)
  (let-values ([(id rhs) (normalize-definition stx #'lambda #f)])
    (with-syntax ([id id]
                  [rhs rhs])
      (syntax/loc stx
                 (define-syntax id (make-honu-transformer rhs))))))

(define-syntax (define-honu-operator/syntax stx)
  (syntax-parse stx
    [(_ name precedence associativity binary-function)
     (syntax/loc stx (define-syntax name (make-honu-operator precedence associativity binary-function #f #f)))]
    [(_ name precedence associativity binary-function unary-function postfix?)
     (syntax/loc stx (define-syntax name (make-honu-operator precedence associativity binary-function unary-function postfix?)))]))

(define-syntax (define-honu-fixture stx)
  (syntax-parse stx
    [(_ name transformer)
     #'(define-syntax name (make-fixture transformer))]))

(define-syntax-rule (define-binary-operator name precedence associativity operator)
    (define-honu-operator/syntax name precedence associativity
                                 ;; binary
                                 (lambda (left right)
                                   (with-syntax ([left left]
                                                 [right right])
                                     (racket-syntax (operator left right))))))

(define-syntax-rule (define-unary+binary-operator name precedence associativity operator)
    (define-honu-operator/syntax name precedence associativity
                                 ;; binary
                                 (lambda (left right)
                                   (with-syntax ([left left]
                                                 [right right])
                                     (racket-syntax (operator left right))))
                                 ;; unary
                                 (lambda (arg)
                                   (with-syntax ([arg arg])
                                     (racket-syntax (operator arg))))
                                 ;; binary operators should not be able to be postfix
                                 #f))

(define-syntax-rule (define-unary-operator name precedence postfix? operator)
                    ;; associativity dont matter for unary 
                    (define-honu-operator/syntax name precedence 'left
                                                 #f
                                                 ;; unary
                                                 (lambda (argument)
                                                   (with-syntax ([argument argument])
                                                     (racket-syntax (operator argument))))
                                                 postfix?))

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
