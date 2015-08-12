#lang info

(define collection 'multi)
(define deps '("scheme-lib"
               "macro-debugger"
               "base"
               "parser-tools-lib"
               "rackunit-lib"))
(define build-deps '("racket-index"
                     "scribble-lib"
                     "at-exp-lib"
                     "sandbox-lib"
                     "racket-doc"))

;; Make honu.vim easier to find by copying it to the "share" directory:

(define pkg-desc "The implementation of the Honu language")

(define pkg-authors '(mflatt rafkind))
