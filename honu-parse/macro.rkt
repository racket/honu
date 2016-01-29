#lang racket
(require (for-syntax racket/base
                     syntax/define
                     syntax/parse
                     "compile.rkt"
                     "transformer.rkt"
                     "parse.rkt"))

(begin-for-syntax
  (define (split stx)
    (syntax-parse stx
      [(o t ...) (values #'o #'(t ...))]))

  (define (do-macro stream precedence left current)
    (define (definition? code)
      (define (contains-define? code)
        (syntax-parse code #:literals (define define-honu-syntax)
                      [(define x ...) #t]
                      [(define-honu-syntax x ...) #t]
                      [else #f]))
      (and (parsed-syntax? code)
           (contains-define? code)))
    (define-values (head rest) (split stream))
    (if current
        (values (left current) stream)
        (begin
          (let-values ([(parsed unparsed terminate?)
                        ((syntax-local-value head)
                         (with-syntax ([head head]
                                       [(rest ...) rest])
                           (datum->syntax #'head
                                          (syntax->list #'(head rest ...))
                                          #'head #'head)))])
            (with-syntax ([parsed parsed]
                          [rest unparsed])
              (define re-parse
                #'parsed)
              (define terminate (definition? re-parse))
              (if terminate
                  (values (left re-parse)
                          #'rest)
                  (do-parse #'rest precedence
                            left re-parse)))))))

  
  (define (do-fixture stream precedence left current)
    (define-values (head rest) (split stream))
    (define transformer (honu-fixture-proc (syntax-local-value #'head)))
    (define-values (output rest*) (transformer current stream))
    (do-parse rest* precedence left output))

  (struct honu-macro (proc)
    #:property prop:honu-transformer do-macro
    #:property prop:procedure 0)

  (struct honu-fixture (proc)
    #:property prop:honu-transformer do-fixture)
  
)

(define-syntax (define-honu-syntax stx)
  (let-values ([(id rhs) (normalize-definition stx #'lambda #f)])
    (with-syntax ([id id]
                  [rhs rhs])
      (syntax/loc stx
        (define-syntax id (honu-macro rhs))))))

(define-syntax (define-honu-fixture stx)
  (syntax-parse stx
    [(_ name transformer)
     #'(define-syntax name (honu-fixture transformer))]))

(provide define-honu-syntax define-honu-fixture)
