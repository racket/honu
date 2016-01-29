#lang racket/base

(define-syntax-rule (standard-honu meta-level)
  (begin
    (require (for-meta meta-level
                       (only-in racket/base #%app)
                       racket/base
                       racket/class
                       (prefix-in list: racket/list)
                       "private/macro.rkt"
                       "private/class.rkt"
                       "private/operator.rkt"
                       honu-parse/syntax
                       (prefix-in literal: "private/literals.rkt")
                       (prefix-in literal: honu-parse/literals)
                       (prefix-in syntax-parse: syntax/parse)
                       (prefix-in racket: racket/base)
                       (prefix-in parse: honu-parse/parse)
                       "private/honu.rkt"))
    (provide (for-meta meta-level 
                       (rename-out [parse:honu-expression expression]
                                   [parse:honu-expression-list expression_list]
                                   [parse:honu-identifier identifier]
                                   [racket:else else]
                                   [racket:void void]
                                   [parse:honu-number number]
                                   [parse:honu-string string]
                                   [honu-function function]
                                   [honu-function fun]
                                   [honu-var var]
                                   [honu-syntax-var syntax_var]
                                   [honu-== ==]
                                   [honu-not-equal !=]
                                   [honu-class class]
                                   [honu-require require]
                                   [honu-provide provide]
                                   [honu-new new]
                                   [honu-while while]
                                   [honu-macro macro]
                                   [honu-phase phase]
                                   [honu-racket racket]
                                   [honu-primitive-macro primitive_macro]
                                   [honu-pattern pattern]
                                   [racket:read-line readLine]
                                   [honu-with-input-from-file withInputFromFile]
                                   [define-make-honu-operator operator]
                                   [define-make-honu-binary-operator binary_operator]
                                   [define-make-honu-unary-operator unary_operator]
                                   [honu-match match]
                                   [honu-with with]
                                   [literal:honu-where where]
                                   [honu-for-syntax for_syntax]
                                   [honu-var var]
                                   [honu-val val]
                                   [honu-for for]
                                   [honu-fold fold]
                                   [honu-to to]
                                   [honu-if if]
                                   [honu-quote quote]
                                   [honu-quasiquote quasiquote]
                                   [honu-+ +] [honu-- -]
                                   [honu-* *] [honu-/ /]
                                   [honu-modulo %]
                                   [honu-^ ^]
                                   [honu-> >] [honu-< <]
                                   [honu->= >=]
                                   [honu-<= <=]
                                   [honu-=> =>]
                                   [honu-map map]
                                   [honu-flow \|]
                                   [honu-dot %dot]
                                   [honu-string=? string_equal]
                                   [honu-cons ::]
                                   [honu-and and] [honu-and &&]
                                   [honu-or or] [honu-or \|\|]
                                   [honu-not not] [honu-not !]
                                   [honu-structure structure]
                                   [honu-structure struct]
                                   [honu-syntax syntax]
                                   [honu-equal =]
                                   [honu-+= +=] [honu--= -=]
                                   [honu-*= *=] [honu-/= /=]
                                   [literal:postfix postfix]
                                   [literal:honu-prefix prefix]
                                   [literal:honu-rename rename]
                                   [literal:honu-then then]
                                   [literal:%colon %colon]
                                   [literal:honu-in in]
                                   [literal:%semicolon %semicolon]
                                   [literal:honu-comma honu-comma]
                                   [literal:honu-$ honu-$]
                                   [literal:honu-<- <-]
                                   [honu--> ->]
                                   [literal:honu-in-lines inLines]
                                   [literal:#%brackets #%brackets]
                                   [literal:#%braces #%braces]
                                   [literal:#%parens #%parens])
                       (rename-out
                         [datum->syntax datum_to_syntax]
                         [syntax->datum syntax_to_datum]
                         [syntax->list syntax_to_list]
                         [list:first first]
                         [symbol->string symbol_to_string]
                         [string-append string_append])
                       print printf
                       true false
                       withSyntax
                       mergeSyntax
                       this
                       error
                       #%app
                       #%top
                       #%datum
                       (... ...)
                       ))))

(require "private/honu-top.rkt")

;; Provide standard stuff at phase 1
(standard-honu 1)
(standard-honu 0)

(provide (rename-out [#%dynamic-honu-module-begin #%module-begin]
                     [honu-top-interaction #%top-interaction]))

