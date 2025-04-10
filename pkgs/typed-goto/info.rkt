#lang info

(define license 'MIT)
(define collection "typed")
(define version "1.1")

(define pkg-desc "Typed goto based on call/cc")

(define deps '("base" "typed-racket-lib" ["goto" #:version "1.1"]))
(define implies '("goto"))

(define clean '("compiled" "private/compiled"))
(define test-omit-paths '(#px"^((?!/tests/).)*$"))
