#lang racket/base

(provide (all-defined-out))

(require (for-syntax racket/base
                     syntax/define)
;         "def-forms.rkt"
         "macro.rkt"
         "operator.rkt")

;; Do any honu-specific expansion here
(require (for-syntax
           "template.rkt" ;; for compress-dollars at phase 1
           "compile.rkt"
           "literals.rkt"
           syntax/stx
           syntax/parse)
         "template.rkt") ;; for remove-repeats at phase 0
(define-honu-syntax honu-syntax
  (lambda (code)
    (syntax-parse code #:literal-sets (cruft)
      [(_ (#%parens stuff ...) . rest)
       (define context (stx-car #'(stuff ...)))
       (define compressed (compress-dollars #'(stuff ...)))
       (values
         (with-syntax ([stuff* (datum->syntax context
                                              (syntax->list compressed)
                                              context context)])
           ;; (debug "Stuff is ~a\n" (syntax->datum #'stuff*))
           ;; (debug "Stuff syntaxed is ~a\n" (syntax->datum #'#'stuff*))

           ;; stuff* will be expanded when this syntax is returned because
           ;; the whole thing will be
           ;;   (remove-repeats #'((repeat$ 1) (repeat$ 2)))
           ;; so remove-repeats will be executed later
           (racket-syntax
             (remove-repeats #'stuff*))) #'rest #f)])))
