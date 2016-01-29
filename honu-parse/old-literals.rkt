#lang racket/base

(provide (all-defined-out))
(require "define.rkt" syntax/parse)



(define-literal honu-comma
                #%braces #%brackets #%parens %colon
                %semicolon
                ellipses-comma ellipses-comma* ellipses-repeat
                honu-$
                ;; FIXME: in-lines should probably not be here
                honu-in-lines
                postfix)

(define-literal-set cruft (#%parens #%brackets #%braces
                                    %semicolon %colon honu-comma))

(define-literal-set base-cruft (%colon honu-comma))
