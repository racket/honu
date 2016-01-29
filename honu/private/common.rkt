#lang racket/base

(require honu-parse
         (for-syntax syntax/parse
                     racket/base
                     honu-parse))

(provide sqr)
(define (sqr x) (* x x))

;; convert a float to an integer
(provide integer)
(define (integer x)
  (inexact->exact (round x)))

(provide honu-cond)
(define-honu-syntax honu-cond
  (lambda (code)
    (syntax-parse code #:literal-sets (cruft)
      [(_ (~seq clause:honu-expression colon body:honu-expression (~optional honu-comma)) ...
          . rest)
       (values
         (racket-syntax (cond
                          [clause.result body.result]
                          ...))
         #'rest
         #t)])))

(provide honu-time)
(define-honu-syntax honu-time
  (lambda (code)
    (syntax-parse code #:literal-sets (cruft)
      [(_ e:honu-expression . rest)
       (values (racket-syntax (time e.result))
               #'rest
               #'t)])))
