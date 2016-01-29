#lang racket/base

(require honu-parse
         (for-syntax honu-parse)
         (for-syntax racket/base)
         (for-syntax (prefix-in parse: syntax/parse))
         (prefix-in parse: syntax/parse))

(define-honu-syntax syntax-parse
  (lambda (code)

    (parse:define-splicing-syntax-class a-pattern #:literals (cruft)
      [parse:pattern (parse:~seq var:parse:id %colon class:parse:id)
                     #:with pattern #'(parse:~var var class #:attr-name-separator "_")])

    (parse:syntax-parse code #:literals (cruft)
      [(_ data:honu-expression (#%braces (#%brackets something:a-pattern action:honu-delayed) ...) . rest)
       (define output
         (racket-syntax (parse:syntax-parse data.result
                           [(something.pattern) action.result] ...)))
       (values output #'rest #t)])))

(provide syntax-parse)
