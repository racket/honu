#lang racket/base


(define-syntax-rule (require-syntax stuff ...)
                    (require (for-syntax stuff ...)))

;; phase 0
(require (for-template racket/base
                       racket/sequence
                       "def-forms.rkt")
         macro-debugger/emit
         syntax/parse
         syntax/parse/experimental/splicing
         syntax/stx
         "compile.rkt"
         "debug.rkt"
         "transformer.rkt"
         "def-forms.rkt"
         "literals.rkt"
         "template.rkt")

;; phase 1
(require-syntax racket/base
                "compile.rkt"
                "debug.rkt")

;; phase -1

(provide (all-defined-out))
(define (strip-stops code)
  (define-syntax-class stopper #:literal-sets (cruft)
    [pattern honu-comma]
    [pattern %colon]) code)

(define (get-value what)
  (syntax-local-value what (lambda () #f)))


(define (bound-to-operator? check)
  (let ([value (get-value check)])
    (debug 2 "operator? ~a ~a\n" check value)
    (operator? value)))

(define (bound-to-fixture? check)
  (let ([value (get-value check)])
    (debug 2 "fixture? ~a ~a\n" check value)
    (fixture? value)))

(define (bound-to-macro? check)
  (let ([value (get-value check)])
    (debug 2 "macro? ~a ~a\n" check value)
    (honu-transformer? value)))

(define (honu-macro? something)
  (and (identifier? something)
       (bound-to-macro? something)))

(define (honu-operator? something)
  (and (identifier? something)
       (bound-to-operator? something)))

(define (honu-fixture? something)
  (and (identifier? something)
       (bound-to-fixture? something)))

(define (semicolon? what)
  (define-literal-set check (%semicolon))
  (define is (and (identifier? what)
                    ((literal-set->predicate check) what)))
  (debug "Semicolon? ~a ~a\n" what is)
  is)

(define (comma? what)
  (define-literal-set check (honu-comma))
  (define is (and (identifier? what)
                    ((literal-set->predicate check) what)))
  (debug 2 "Comma? ~a ~a\n" what is)
  is)

(define-literal-set argument-stuff [honu-comma])

(define (stopper? what)
  (define-literal-set check (honu-comma %semicolon %colon))
  (define is (and (identifier? what)
                  ((literal-set->predicate check) what)))
  (debug 2 "Comma? ~a ~a\n" what is)
  is)
