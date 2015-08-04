#lang racket/base

(require (for-syntax scheme/base
                     syntax/stx
                     syntax/name
                     syntax/define
                     syntax/parse
                     syntax/parse/experimental/splicing
                     scheme/splicing
                     macro-debugger/emit
                     racket/pretty
                     "compile.rkt"
                     "debug.rkt"
                     "contexts.rkt"
                     "util.rkt"
                     "syntax.rkt"
                     "parse.rkt"
                     "literals.rkt"
                     )
         syntax/parse
         "literals.rkt"
         "debug.rkt")

(require (for-meta 2 racket/base "util.rkt"))
(require (for-meta 3 racket/base))

(provide (all-defined-out))

(begin-for-syntax
  (define (expression-result ctx expr rest)
    (if (top-block-context? ctx)
        (values #`(#%expression (show-top-result #,expr)) rest)
        (values #`(#%expression #,expr) rest)))
  
  )

#|

Yes, check out head patterns and splicing syntax classes.

For example, if 'display' is a special kind of statement, you might have something like this:

(define-splicing-syntax-class statement
      (pattern (~seq (~literal display) something (~literal \;)))
      ___ <other alternatives> ___)

Then, in the pattern above for 'if', 'then' would be bound to the following syntax list:
  (display (#%braces "hello world") \;) 

(if expr block else statement rest)
(if expr block rest)

|#

(define-syntax (define-honu-infix-syntax stx)
  (let-values ([(id rhs) (normalize-definition stx #'lambda #f)])
    (with-syntax ([id id]
                  [rhs rhs])
      (syntax/loc stx
        (define-syntax id (make-honu-infix-transformer rhs))))))

(define (show-top-result v)
  (unless (void? v)
    (debug "~s\n" v)))

(define-syntax (op-app stx)
  (syntax-case stx (#%parens #%angles)
    [(_ #%parens a (b ...))
     #'(a b ...)]
    [(_ #%angles a (b ...))
     #'(honu-type-app a b ...)]
    [(_ a b ...) 
     (datum->syntax #'a
                    (cons #'a #'(b ...))
                    #'a)]))

(define-syntax (honu-top stx)
  (debug "Honu ~a\n" (syntax->datum stx))
  (raise-syntax-error #f "interactive use is not yet supported"))

(define (display2 x y)
  (debug "~a ~a" x y))

(define-for-syntax (honu-expand forms)
  (parse-one forms))

(define-for-syntax (honu-compile forms)
  #'(void))

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
     #;
     (begin
       (debug "expanded ~a unexpanded ~a\n"
              (if parsed (syntax->datum parsed) parsed)
              (syntax->datum unparsed))(define-values (parsed unparsed) (honu-expand #'(forms ...)))
                                       (with-syntax ([parsed (if (not parsed) #'(begin)
                                                                 (remove-repeats parsed)
                                                                 #;
                                                                 (honu->racket parsed))]
                                                     [(unparsed ...) unparsed])
                                         (debug "Final parsed syntax\n~a\n" (pretty-format (syntax->datum #'parsed)))
                                         (debug "Unparsed syntax ~a\n" #'(unparsed ...))
                                         (if (null? (syntax->datum #'(unparsed ...)))
                                             (if (parsed-syntax? #'parsed)
                                                 #'parsed
                                                 (with-syntax ([(out ...) #'parsed])
                                                   #'(honu-unparsed-begin out ...)))
                                             (if (parsed-syntax? #'parsed)
                                                 #'(begin parsed (honu-unparsed-begin unparsed ...))
                                                 (with-syntax ([(out ...) #'parsed])
                                                   #'(honu-unparsed-begin out ... unparsed ...))))))]))

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

