#lang racket/base
;; phase 0
(require (for-template racket/base
                       racket/sequence)
         (for-syntax racket/base
                     "compile.rkt"
                     "debug.rkt")
         macro-debugger/emit
         syntax/parse
         syntax/parse/experimental/splicing
         syntax/stx
         "compile.rkt"
         "debug.rkt"
         "transformer.rkt"
         "old-literals.rkt")

(provide parse parse-all parse-all-expression parse-comma-expression do-parse parse-delayed)

(define (strip-stops code)
  (define-syntax-class stopper #:literals (%colon honu-comma)
    [pattern honu-comma]
    [pattern %colon]) code)


(define (get-value what)
  (syntax-local-value what (lambda () #f)))

(define (bound-to-transformer? check)
  (let ([value (get-value check)])
    (honu-transformer? value)))

(define (bound-to-wrap? check)
  (let ([value (get-value check)])
    (honu-wrap-transformer? value)))

(define (is-honu-transformer? something)
  (and (identifier? something)
       (bound-to-transformer? something)))

(define (is-honu-wrap? something)
  (and (identifier? something)
       (bound-to-wrap? something)))

(define-literal-set argument-stuff [honu-comma])

(define (stopper? what)
  (define-literal-set check (honu-comma %semicolon %colon))
  (define is (and (identifier? what)
                  ((literal-set->predicate check) what)))
  (debug 2 "Comma? ~a ~a\n" what is)
  is)


(define (parse-arguments arguments)
  (define-syntax-class val
    [pattern x:identifier #:when (equal? 'val (syntax-e #'x))])
  (let loop ([out '()]
             [arguments arguments])
    (syntax-parse arguments
                  #:datum-literals (honu-comma)
      [(x:val name:identifier honu-comma more ...)
       (loop (cons #'name out) #'(more ...))]
      [(name:identifier honu-comma more ...)
       (loop (cons #'name out) #'(more ...))]
      [(x:val name:identifier)
       (loop (cons #'name out) #'())]
      [(name:identifier)
       (loop (cons #'name out) #'())]
      [() (reverse out)])))

(define (parse-comma-expression arguments)
  (if (null? (syntax->list arguments))
    '()
    (let loop ([used '()]
               [rest arguments])
      (if (empty-syntax? rest)
        (reverse used)
        (syntax-parse rest   #:datum-literals (honu-comma)
          [(honu-comma more ...)
           (loop used #'(more ...))]
          [else
            (let-values ([(parsed unparsed)
                          ;; FIXME: don't strip all stops, just comma
                          (parse (strip-stops rest))])
              (loop (if parsed
                      (cons (parse-all-expression parsed) used)
                      used)
                    unparsed))])))))

(provide do-parse-rest)
(define (do-parse-rest stx parse-more)
  (syntax-parse stx   #:datum-literals (#%parens #%brackets #%braces %semicolon %colon honu-comma)
    [(stuff ...)
     (debug "Parse rest ~a\n" (syntax->datum #'(stuff ...)))
     (define-values (parsed unparsed)
                    (parse (strip-stops #'(stuff ...))))
     (debug "Parse more: ~a unparsed ~a\n" parsed unparsed)
     (define output (if parsed parsed #'(void)))
     (debug "Output ~a unparsed ~a\n"
            (syntax->datum output)
            (syntax->datum unparsed))
     (with-syntax ([output output]
                   [(unparsed-out ...) unparsed]
                   [parse-more parse-more])
       (if (null? (syntax->datum #'(unparsed-out ...)))
         (if (parsed-syntax? #'output)
           #'output
           (with-syntax ([(out ...) #'output])
             #'(parse-more out ...)))
         (if (parsed-syntax? #'output)
           #'(begin output (parse-more unparsed-out ...))
           (with-syntax ([(out ...) #'output])
             #'(begin (parse-more out ...) (parse-more unparsed-out ...))))))]
    [() #'(begin)]))

(define (do-parse-rest/local stx)
  (define name (gensym 'local-parser))
  (define local-parser (with-syntax ([name name])
                         #'(define-syntax (name stx)
                             (syntax-case stx ()
                               [(_ stuff (... ...))
                                (debug "Properties on first element ~a\n" (syntax-property-symbol-keys (stx-car #'(stuff (... ...)))))
                                (do-parse-rest #'(stuff (... ...)) #'name)]))))
  (with-syntax ([local local-parser])
    (with-syntax ([stx stx]
                  [name name])
      (debug "Create local parser for ~a properties ~a\n" (syntax->datum #'stx) (syntax-property-symbol-keys #'stx))
      ;; sort of a hack, if the input is already parsed then don't deconstruct it
      ;; otherwise the input is a honu expression so we need to splice it in
      (define with-local
        (if (parsed-syntax? #'stx)
          #'(begin local (unexpand-honu-syntax (name stx)))
          (with-syntax ([(inside ...) #'stx])
            #'(begin local (unexpand-honu-syntax (name inside ...))))))
      (emit-local-step #'stx with-local #:id #'do-parse-rest/local)
      (parsed-syntax with-local))))

(define-syntax-rule (parse-delayed code ...)
                    (let ()
                      (define-syntax (parse-more stx)
                        (syntax-case stx ()
                          [(_ stuff (... ...))
                           (racket-syntax (do-parse-rest #'(stuff (... ...)) #'parse-more))]))
                      (parse-more code ...)))

(provide honu-body)
(define-syntax-class honu-body
    #:datum-literals (#%parens #%brackets #%braces %semicolon %colon honu-comma)
  [pattern (#%braces code ...)
           #:with result
           (racket-syntax (let ()
                            (define-syntax (parse-more stx)
                              (syntax-case stx ()
                                [(_ stuff (... ...))
                                 (do-parse-rest #'(stuff (... ...)) #'parse-more)]))
                            (parse-more code ...)))])

(provide honu-delayed)
(define-syntax-class honu-delayed
  [pattern any #:with result (racket-syntax
                               (let ()
                                 (define-syntax (parse-more stx)
                                   (syntax-case stx ()
                                     [(_ stuff (... ...))
                                      (do-parse-rest #'(stuff (... ...)) #'parse-more)]))
                                 (parse-more any)))])

(provide honu-function-pattern)
(define-splicing-syntax-class honu-function-pattern
  [pattern (~seq function:identifier ((~datum #%parens) args ...) body:honu-body)
           #:with result
           (with-syntax ([(parsed-arguments ...)
                          (parse-arguments #'(args ...))])
             (racket-syntax (define (function parsed-arguments ...)
                              body.result)))])

(define-syntax-class atom
  [pattern x:identifier #:when (not (free-identifier=? #'#%braces #'x))]
  [pattern x:str]
  [pattern x:number])

(define-syntax-class parsed-expr
  [pattern e #:when (parsed-syntax? #'e)])

(define-syntax-class transformer-expr
  [pattern e #:when (is-honu-transformer? #'e)])
(define-syntax-class wrap-expr
  [pattern e #:when (is-honu-wrap? #'e)])


(define (do-parse stream precedence left current)
  (debug "parse ~a precedence ~a left ~a current ~a properties ~a\n"
         (syntax->datum stream) precedence left current
         (syntax-property-symbol-keys stream))
  (define final (if current current (racket-syntax (void))))
  (syntax-parse stream
    [s:parsed-expr (values (left stream) #'())]
    [(t:transformer-expr rest ...)
     (let* ([t-val (syntax-local-value #'t (λ()#t))]
            [t-fn ((honu-transformer t-val) t-val)])
       (t-fn stream precedence left current))]
    [((w:wrap-expr stuff ...) rest ...)
     (let* ([w-val (syntax-local-value #'w (λ()#t))]
            [w-fn ((honu-wrap-transformer w-val) w-val)])
       (w-fn stream precedence left current))]
    [()
     (debug "Empty input out: left ~a ~a\n" left (left final))
     (values (left final) #'())]
    [(head:parsed-expr rest ...)
     (debug "Parsed syntax ~a\n" #'head)
     (emit-local-step #'head #'head #:id #'do-parse)
     (if current
         (values current stream)
         (do-parse #'(rest ...) precedence left #'head))]
    [(x:atom rest ...)
     (debug 2 "atom ~a current ~a\n" #'x current)
     (if current
         (values (left current) stream)
         (do-parse #'(rest ...) precedence left (racket-syntax x)))]
    [(head stuff ...) (error 'parser "don't know how to parse ~a" #'head)]))


(define (parse input)
  (emit-remark "Honu parse" input)
  (define-values (parsed unparsed)
                 (do-parse input 0 (lambda (x) x) #f))
  (values parsed unparsed))

(define (empty-syntax? what)
  (syntax-parse what
    [() #t]
    [else #f]))

;; parse one form
;; return the parsed stuff and the unparsed stuff
(provide parse-one)
(define (parse-one code)
  (parse (strip-stops code)))

;; keep parsing some expression until only a parsed term remains
(define (parse-all-expression code)
  (define-values (parsed unparsed)
                 (parse code))
  (when (not (empty-syntax? unparsed))
    (raise-syntax-error 'parse-all-expression "expected no more syntax" code))
  (if (parsed-syntax? parsed)
    parsed
    (parse-all-expression parsed)))

(define (parse-all code)
  (let loop ([all '()]
             [code code])
    (define-values (parsed-original unparsed)
                   (parse (strip-stops code)))
    (define parsed (if (parsed-syntax? parsed-original)
                     parsed-original
                     (let-values ([(out rest) (parse parsed-original)])
                       (when (not (empty-syntax? rest))
                         (raise-syntax-error 'parse-all "expected no more syntax" parsed-original))
                       out)))
    (debug "Parsed ~a unparsed ~a all ~a\n"
           (if parsed (syntax->datum parsed) parsed)
           (if unparsed (syntax->datum unparsed) unparsed)
           all)
    (if (empty-syntax? unparsed)
      (with-syntax ([(use ...) (reverse (if parsed
                                          (cons parsed all)
                                          all))])
        (debug "Nothing left to parse. Use ~a\n" #'(use ...))
        ;; If multiple things then wrap inside a begin
        (syntax-parse #'(use ...)
          [(x z y ...)
           (emit-remark "Parsed all" #'(begin use ...))
           (racket-syntax (begin use ...))]
          [(x) (racket-syntax x)]))
      (loop (cons parsed all)
            unparsed))))

(provide parsed-things)
;; rest will be some subset of full
(define (parsed-things full rest)
  (define full-datum (syntax->datum full))
  (define rest-datum (syntax->datum rest))
  (- (length full-datum) (length rest-datum)))

(provide honu-expression)
(define-primitive-splicing-syntax-class (honu-expression)
  #:attributes (result)
  #:description "expression"
  (lambda (stx fail)
    (define context (gensym))
    (debug "[~a] honu expression syntax class on ~a\n" context stx)
    (if (or (stx-null? stx))
      (begin
        (debug "[~a] failed\n" context)
        (fail))
      (let ()
        (define mark (make-syntax-introducer))
        ;; mark the syntax that is passed to `parse', then re-mark the parsed
        ;; object that comes out and the unparsed object
        (define-values (parsed unparsed)
                       (call-with-values 
                         (lambda ()
                           (parse (mark stx)))
                         (lambda (parsed unparsed)
                           (values (mark parsed)
                                   (mark unparsed)))))
        (emit-remark "honu-expression class parsed" parsed)
        (debug "[~a] expression parsed ~a. Parsed? ~a\n" context (if parsed (syntax->datum parsed) parsed) (parsed-syntax? parsed))
        (debug 2 "[~a] Parsed things ~a\n" context (parsed-things stx unparsed))
        (if (parsed-syntax? parsed)
          (list (parsed-things stx unparsed)
                parsed)
          ;; if the parsed thing still needs to be parsed more it probably doesn't
          ;; need to be remarked
          (list (parsed-things stx unparsed)
                (parse-all parsed)))))))

(provide honu-expression-list)
(define-splicing-syntax-class (honu-expression-list)
    #:datum-literals (#%parens #%brackets #%braces %semicolon %colon honu-comma)
  [pattern (~seq (~seq each:honu-expression (~optional honu-comma)) ...)
           #:with (each_result ...)
           #'(each.result ...)])

(provide honu-identifier)
(define-splicing-syntax-class honu-identifier
                                #:datum-literals (#%parens #%brackets #%braces %semicolon %colon honu-comma)
  [pattern (~and (~not %semicolon)
                 x:id) #:with result #'x])

(provide honu-number)
(define-splicing-syntax-class honu-number
                                #:datum-literals (#%parens #%brackets #%braces %semicolon %colon honu-comma)
  [pattern x:number #:with result #'x])

(provide honu-string)
(define-splicing-syntax-class honu-string
                                #:datum-literals (#%parens #%brackets #%braces %semicolon %colon honu-comma)
  [pattern x:str #:with result #'x])

(provide identifier-comma-list)
(define-splicing-syntax-class identifier-comma-list
                                #:datum-literals (#%parens #%brackets #%braces %semicolon %colon honu-comma)
  [pattern (~seq (~seq name:id (~optional honu-comma) ...) ...)]) 

(provide honu-expression/comma)
(define-splicing-syntax-class honu-expression/comma
  [pattern (~seq x ...) #:with (result ...) (parse-comma-expression #'(x ...))])
