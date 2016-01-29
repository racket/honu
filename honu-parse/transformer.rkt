#lang racket/base
(require racket/syntax)

(provide prop:honu-transformer honu-transformer? honu-transformer)
(define-values (prop:honu-transformer honu-transformer? honu-transformer)
  (make-struct-type-property
   'honu-transformer
   (lambda(v sti)(if (procedure? v)(λ(s) v) (make-struct-field-accessor (list-ref sti 3) v)))))

(provide prop:honu-wrap-transformer honu-wrap-transformer? honu-wrap-transformer)
(define-values (prop:honu-wrap-transformer honu-wrap-transformer? honu-wrap-transformer)
  (make-struct-type-property
   'honu-wrap-transformer
   (lambda(v sti)(if (procedure? v)(λ(s) v) (make-struct-field-accessor (list-ref sti 3) v)))))

(struct primitive-honu-transformer (proc)
  #:property prop:honu-transformer (struct-field-index proc))

(struct primitive-honu-wrap (proc)
  #:property prop:honu-wrap-transformer (struct-field-index proc))

(struct primitive-honu-wrap+transformer (proc)
  #:property prop:honu-transformer (struct-field-index proc)
  #:property prop:honu-wrap-transformer (struct-field-index proc))

(provide primitive-honu-transformer primitive-honu-wrap primitive-honu-wrap+transformer)

