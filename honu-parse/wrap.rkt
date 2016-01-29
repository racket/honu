#lang racket
(require (for-syntax racket/base
                     syntax/define
                     syntax/parse
                     "debug.rkt"
                     "compile.rkt"
                     "transformer.rkt"
                     "parse.rkt"))

(define-syntax (define-honu-wrap stx)
  (syntax-parse stx
    [(_ id:id fn)
     #'(define-syntax id (primitive-honu-wrap fn))]
    [(_ (id:id args ...) body ...)
     #'(define-syntax id (primitive-honu-wrap (lambda(args ...) body ...)))]))

(define-syntax (define-honu-wrap+tranformer stx)
  (syntax-parse stx
    [(_ id:id fn)
     #'(define-syntax id (primitive-honu-wrap+transformer fn))]
    [(_ (id:id args ...) body ...)
     #'(define-syntax id (primitive-honu-wrap+transformer (lambda(args ...) body ...)))]))


(define-honu-wrap (#%parens stream precedence left current)
  (define-values (head args rest)
    (syntax-parse stream [((~and h (_ stuff ...)) rest ...)
                          (values #'h #'(stuff ...) #'(rest ...))]))
  (define (empty-syntax? what)
    (syntax-parse what
      [() #t]
      [else #f]))
  (if current
      ;; FIXME: 9000 is an arbitrary precedence level for
      ;; function calls
      (with-syntax ([app (datum->syntax head '#%app)])
        (if (> precedence 9000)
            (let ()
              (debug 2 "higher precedence call ~a\n" current)
              (define call (with-syntax ([current (left current)]
                                         [(parsed-args ...)
                                          (parse-comma-expression args) ])
                             (racket-syntax (app current parsed-args ...))))
              (do-parse rest 9000 (lambda (x) x) call))
            (let ()
              (debug 2 "function call ~a\n" left)
              (define call (with-syntax ([current current]
                                         [(parsed-args ...)
                                          (parse-comma-expression args) ])
                             (debug "Parsed args ~a\n" #'(parsed-args ...))
                             (racket-syntax (app current parsed-args ...))))
              (do-parse rest precedence left call))))
      (let ()
        (debug "inner expression ~a\n" args)
        (define-values (inner-expression unparsed) (parse args))
        (when (not (empty-syntax? unparsed))
          (error 'parse "expression had unparsed elements ~a" unparsed))
        (do-parse rest precedence left inner-expression))))


(define-honu-wrap (#%brackets stream precedence left current)
  (define-values (stuff rest)
    (syntax-parse stream [((_ stuff ...) rest ...)
                          (values #'(stuff ...) #'(rest ...))]))
  (syntax-parse stuff   #:datum-literals (#%parens #%brackets #%braces %semicolon %colon honu-comma)
    [(work:honu-expression 
      %colon (~seq variable:id (~datum =) list:honu-expression (~optional honu-comma)) ...
      (~seq (~datum where) where:honu-expression (~optional honu-comma)) ...)
     (define filter (if (attribute where)
                        #'((#:when where.result) ...)
                        #'()))
     (define comprehension
       (with-syntax ([((filter ...) ...) filter])
         (racket-syntax (for/list ([variable list.result]
                                   ...
                                   filter ... ...)
                          work.result))))
     (if current
         (values (left current) stream)
         (do-parse rest precedence left comprehension))]
    [else
     (debug "Current is ~a\n" current)
     (define value (with-syntax ([(data ...)
                                  (parse-comma-expression stuff)])
                     (debug "Create list from ~a\n" #'(data ...))
                     (racket-syntax (list data ...))))
     (define lookup (with-syntax ([(data ...)
                                   (parse-comma-expression stuff)]
                                  [current current])
                      (racket-syntax (sequence-ref current data ...))))
     (if current
         (do-parse rest precedence left lookup)
         (do-parse rest precedence left value))]))

(define-honu-wrap+tranformer (#%braces stream precedence left current)
    (define (result stx)
      (syntax-parse stx
        [(_ stuff ...)
         (racket-syntax
          (let ()
            (define-syntax (parse-more stx)
              (syntax-case stx ()
                [(_ stuff* (... ...))
                 (do-parse-rest #'(stuff* (... ...)) #'parse-more)]))
            (parse-more stuff ...)))]))
    (syntax-parse stream
      [(_ stuff ...)
       (if current
         (values (left current) stream)
         (values (left (result stream)) #'()))]
      [((_ stuff ...) rest ...)
       (if current
         (values (left current) stream)
         (do-parse #'(rest ...) precedence left (result #'(#%braces stuff ...) )))]))

(define-honu-wrap (%semicolon stream precedence left current)
  (syntax-parse stream
    [((%semicolon inner ...) rest ...)
     ;; nothing on the left side should interact with a semicolon
     (if current
         (values (left current) stream)
         (begin
           (with-syntax ()
             (values (left (parse-delayed inner ...)) #'(rest ...)))))]))

(provide #%parens #%brackets #%braces %semicolon)
