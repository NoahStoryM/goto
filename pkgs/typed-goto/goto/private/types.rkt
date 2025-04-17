#lang typed/racket/base

(define-type (Goto a) (∪ a (→ a Nothing)))
(define-type Label (Goto Label))
(provide Goto Label)
