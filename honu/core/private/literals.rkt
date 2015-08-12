#lang racket/base

(provide (all-defined-out))
(require enforest)

(define-literal
                honu-|| honu-%
                honu-%=
                honu-&= honu-^= honu-\|= honu-<<= honu->>= honu->>>=
                honu->> honu-<< honu->>> 
                honu-!=
                honu-<-
                honu-literal
                honu-then
                honu-?
                honu-:
  ;                honu-comma ;
                 honu-.
;                #%braces #%brackets #%parens %colon
;                %semicolon
;                ellipses-comma ellipses-comma* ellipses-repeat
                honu-in
                honu-where
                honu-for-template
                honu-prefix
                honu-rename
;                honu-$
                ;; FIXME: in-lines should probably not be here
;                honu-in-lines
                ;                postfix
                )
