#lang info

(define license 'MIT)
(define collection "goto")
(define version "1.0")

(define pkg-desc "Goto based on call/cc")

(define deps '("base"))
(define build-deps '("at-exp-lib" "scribble-lib" "rackunit-lib" "racket-doc" "data-doc"))

(define scribblings '(("scribblings/goto.scrbl")))

(define clean '("compiled" "private/compiled"))
(define test-omit-paths '(#px"^((?!/tests/).)*$"))
