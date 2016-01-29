#lang racket/base
(require (for-syntax racket/base)
         "operator.rkt")

(define-unary+binary-operator add 1 'left +)
(define-unary+binary-operator sub 1 'left -)
(define-binary-operator mult 2 'left *)
(define-binary-operator div 2 'left /)
(define-binary-operator pow 2 'right expt)
(define-binary-operator equals 1 'left =)
(define-binary-operator less 0.9 'left <)
(define-binary-operator leq 0.9 'left <=)
(define-binary-operator greater 0.9 'left >)
(define-binary-operator geq 0.9 'left >=)

(provide (rename-out [add +][sub -][mult *][div /]
                     [pow ^][equals =][less <]
                     [leq <=][greater >][geq >=]))
