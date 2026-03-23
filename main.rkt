#lang typed/racket/base/optional

(define-type ⊥ Nothing)
(define-type (¬ a) (→ a ⊥))
(define-type Label (¬ Label))
(define-type (LEM a) (∪ a (¬ a)))
(provide ⊥ ¬ Label LEM)

(require/typed/provide "private/goto.rkt"
  [label (→* () (Prompt-TagTop) Label)]
  [goto (→* (Label) (Label) ⊥)]
  [current-continuation (∀ (a) (case→ (→* () (Prompt-TagTop) (∪ a (¬ a))) (→ (¬ a) a ⊥)))])
(provide (rename-out [current-continuation cc]))
