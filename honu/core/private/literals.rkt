#lang racket/base
(require enforest enforest/literals)
(provide (all-defined-out)
         (all-from-out enforest/literals))

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
