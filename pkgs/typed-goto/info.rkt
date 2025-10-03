#lang info

(define license 'MIT)
(define collection "typed")
(define version "1.0")

(define pkg-desc "Typed goto based on call/cc")

(define deps '("base" "typed-racket-lib" ["goto" #:version "1.0"]))
(define build-deps '("rackunit-typed"))
(define implies '("goto"))

(define clean '("compiled" "private/compiled"))
(define test-omit-paths '(#px"^((?!/tests/).)*$"))
