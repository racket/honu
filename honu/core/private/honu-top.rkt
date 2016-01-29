#lang racket/base

(require (for-syntax macro-debugger/emit
                     scheme/base
                     syntax/define
                     syntax/parse
                     honu-parse)
         (for-meta 2 racket/base)
         (for-meta 3 racket/base)
         honu-parse)

(provide (all-defined-out))

(provide honu-unparsed-begin)
(define-syntax (honu-unparsed-begin stx)
  (emit-remark "Honu unparsed begin!" stx)
  (debug "honu unparsed begin: ~a at phase ~a\n" (syntax->datum stx) (syntax-local-phase-level))
  (syntax-parse stx
    [(_) #'(void)]
    [(_ forms ...)
     #'(begin
         (define-syntax (parse-more stx)
           (syntax-case stx ()
             [(_ stuff (... ...))
              (do-parse-rest #'(stuff (... ...)) #'parse-more)]))
         (parse-more forms ...))
     ;; if parsed is #f then we don't want to expand to anything that will print
     ;; so use an empty form, begin, `parsed' could be #f becuase there was no expression
     ;; in the input such as parsing just ";".
     ]))

(define-syntax (#%dynamic-honu-module-begin stx)
  (syntax-case stx ()
    [(_ forms ...)
     (begin
       (debug "Module begin ~a\n" (pretty-format (syntax->datum #'(forms ...))))
       #'(#%module-begin (honu-unparsed-begin forms ...)))]))

(provide honu-top-interaction)
(define-syntax (honu-top-interaction stx)
  (syntax-case stx ()
    [(_ rest ...)
     #'(#%top-interaction . (honu-unparsed-begin rest ...))]))
