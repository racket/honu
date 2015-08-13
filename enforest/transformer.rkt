#lang racket/base
(require racket/syntax)


(provide honu-transformer? make-honu-transformer honu-trans-ref)

(define-values (prop:honu-transformer honu-transformer? honu-transformer-ref)
               (make-struct-type-property 'honu-transformer))

(define-values (struct:honu-trans make-honu-trans honu-trans? honu-trans-ref honu-trans-set!)
               (make-struct-type 'honu-trans #f 1 0 #f 
                                 (list (list prop:honu-transformer #t))
                                 (current-inspector) 0))

(define (make-honu-transformer proc)
  (unless (and (procedure? proc)
               (procedure-arity-includes? proc 1))
    (raise-type-error
      'define-honu-syntax
      "procedure (arity 1)"
      proc))
  (make-honu-trans proc))

(provide (rename-out [prop:honu-operator? operator?])
         make-honu-operator
         (rename-out [-honu-operator-ref operator-ref]))

(define-values (prop:honu-operator prop:honu-operator? prop:honu-operator-ref)
               (make-struct-type-property 'honu-operator))

(define operator-fields '(precedence assocation binary unary postfix?))

(define-values (struct:honu-operator -make-honu-operator honu-operator? -honu-operator-ref honu-operator-set!)
               (make-struct-type 'honu-operator #f (length operator-fields) 0 #f 
                                 (list (list prop:honu-operator #t))
                                 (current-inspector)
                                 (λ(self stx)
                                   (syntax-case stx ()
                                     [(_ l)
                                      (let ([op-fn (operator-unary-transformer self)])
                                        (op-fn #'l))]
                                     [(_ l r ...)
                                      (let ([op-fn (operator-binary-transformer self)])
                                        (apply op-fn (cons #'l (syntax->list #'(r ...)))))]
                                     ))))

(define (get n)
  (lambda (operator)
    (-honu-operator-ref operator n)))

(provide operator-precedence operator-association
         operator-binary-transformer operator-unary-transformer
         operator-postfix?)

(define operator-precedence (get 0))
(define operator-association (get 1))
(define operator-binary-transformer (get 2))
(define operator-unary-transformer (get 3))
(define operator-postfix? (get 4))

(define (make-honu-operator precedence associativity binary unary postfix?)
  (when (and (procedure? binary)
             (not (procedure-arity-includes? binary 2)))
    (raise-type-error
      'define-honu-operator/syntax
      "procedure (arity 2)"
      binary))
  (when (and (procedure? unary)
             (not (procedure-arity-includes? unary 1)))
    (raise-type-error
      'define-honu-operator/syntax
      "procedure (arity 1)"
      unary))
  (-make-honu-operator precedence associativity binary unary postfix?))

(provide (rename-out [prop:fixture? fixture?])
         make-fixture
         (rename-out [-fixture-ref fixture-ref]))
(define-values (prop:fixture prop:fixture? prop:fixture-ref)
               (make-struct-type-property 'fixture))

(define-values (struct:fixture -make-fixture fixture? -fixture-ref fixture-set!)
               (make-struct-type 'fixture #f 1 0 #f 
                                 (list (list prop:fixture #t))
                                 (current-inspector) 0))

(define (make-fixture transformer)
  (when (and (procedure? transformer)
             (not (procedure-arity-includes? transformer 2)))
    (raise-type-error
      'define-fixture
      "procedure (arity 2)"
      transformer))
  (-make-fixture transformer))
