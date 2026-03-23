#lang info

(define license 'MIT)
(define collection "goto")
(define version "1.2")

(define pkg-desc "Goto based on call/cc")

(define deps '("control-context"))
(define build-deps '())

(define clean '("compiled" "private/compiled"))
(define test-omit-paths '(#px"^((?!/tests/).)*$"))
