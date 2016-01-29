#lang racket
(require (for-syntax racket/base
                     syntax/define
                     syntax/parse
                     "compile.rkt"
                     "transformer.rkt"
                     "parse.rkt"))

;; 1 + 1
;; ^
;;  left: identity
;;  current: 1
;; 1 + 1
;;   ^
;;  left: (lambda (x) (+ 1 x))
;;  current: #f
;; 1 + 1
;;     ^
;;  left: (lambda (x) (+ 1 x))
;;  current: 1
;;
;; 1 + 1 * 2
;;       ^
;;  left: (lambda (x) (left (* 1 x)))
;;  current: #f
;;
;; 1 + 1 * 2
;;         ^
;;  left: (lambda (x) (left (* 1 x)))
;;  current: 2

(begin-for-syntax
  (define (split stx)
    (syntax-parse stx
      [(o t ...) (values #'o #'(t ...))]))
  
  (define (do-operator stream precedence left current)
    (define-values (head rest) (split stream))
    (define operator (syntax-local-value head))
    (define new-precedence (honu-operator-precedence operator))
    (define association (honu-operator-association operator))
    (define binary-transformer (honu-operator-binary-transformer operator))
    (define unary-transformer (honu-operator-unary-transformer operator))
    (define postfix? (honu-operator-postfix? operator))
    (define higher
      (case association
        [(left) >]
        [(right) >=]
        [else (raise-syntax-error 'parse "invalid associativity. must be either 'left or 'right" association)]))
    (if (higher new-precedence precedence)
        (let-values ([(parsed unparsed)
                      (do-parse rest new-precedence
                                (lambda (stuff)
                                  (define right (parse-all stuff))
                                  (define output
                                    (if current
                                        (if binary-transformer
                                            (binary-transformer (parse-all-expression current) right)
                                            ;; use a unary transformer in postfix position
                                            (if (and postfix? unary-transformer)
                                                (unary-transformer current)
                                                (error 'binary "cannot be used as a binary operator in ~a" head)))
                                        (if unary-transformer
                                            (unary-transformer right)
                                            (error 'unary "cannot be used as a unary operator in ~a" head))))
                                  (with-syntax ([out (parse-all output)])
                                    #'out))
                                #f)])
          (do-parse unparsed precedence left parsed))
        ;; if we have a unary transformer then we have to keep parsing
        (if unary-transformer
            (if current
                (if postfix?
                    (do-parse rest
                              precedence
                              left
                              (unary-transformer current))
                    (values (left current) stream))
                (do-parse rest new-precedence
                          (lambda (stuff)
                            (define right (parse-all stuff))
                            (define output (unary-transformer right))
                            ;; apply the left function because
                            ;; we just went ahead with parsing without
                            ;; caring about precedence
                            (with-syntax ([out (left (parse-all output))])
                              #'out))
                          #f))
            ;; otherwise we have a binary transformer (or no transformer..??)
            ;; so we must have made a recursive call to parse, just return the
            ;; left hand
            (values (left current) stream))))

  (struct honu-operator (precedence
                         association
                         binary-transformer
                         unary-transformer
                         postfix?)
    #:property prop:honu-transformer do-operator
    #:property prop:procedure
    (λ(self stx)(syntax-parse stx
                  [(_ l)
                   (let ([op-fn (honu-operator-unary-transformer self)])
                     (op-fn #'l))]
                  [(_ l r ...)
                   (let ([op-fn (honu-operator-binary-transformer self)])
                     (apply op-fn (cons #'l (syntax->list #'(r ...)))))])))

)

(define-syntax (define-honu-operator/syntax stx)
  (syntax-parse stx
    [(_ name precedence associativity binary-function)
     (syntax/loc stx (define-syntax name (honu-operator precedence associativity binary-function #f #f)))]
    [(_ name precedence associativity binary-function unary-function postfix?)
     (syntax/loc stx (define-syntax name (honu-operator precedence associativity binary-function unary-function postfix?)))]))


(define-syntax-rule (define-binary-operator name precedence associativity operator)
  (define-honu-operator/syntax name precedence associativity
    ;; binary
    (lambda (left . right)
      (with-syntax ([left left]
                    [(right (... ...)) right])
        (racket-syntax (operator left right (... ...)))) ) ))

(define-syntax-rule (define-unary+binary-operator name precedence associativity operator)
  (define-honu-operator/syntax name precedence associativity
    ;; binary
    (lambda (left . right)
      (with-syntax ([left left]
                    [(right (... ...)) right])
        (racket-syntax (operator left right (... ...)))) )
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

(provide define-honu-operator/syntax
         define-binary-operator
         define-unary+binary-operator
         define-unary-operator)


