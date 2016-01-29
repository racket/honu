#lang racket/base
(require honu-parse honu-parse/literals)
(provide (all-defined-out)
         (all-from-out honu-parse/literals))

(define-literal honu-|| honu-%
                honu-%=
                honu-&= honu-^= honu-\|= honu-<<= honu->>= honu->>>=
                honu->> honu-<< honu->>> 
                honu-!=
                honu-<-
                honu-literal
                honu-then
                honu-?
                honu-:
                honu-.
                honu-in
                honu-where
                honu-for-template
                honu-prefix
                honu-rename)
