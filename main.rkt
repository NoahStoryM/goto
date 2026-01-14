#lang typed/racket/base/optional

(define-type ⊥ Nothing)
(define-type (¬ a) (→ a ⊥))
(define-type Label (¬ Label))
(provide ⊥ ¬ Label)

(require/typed/provide "private/goto.rkt"
  [label (∀ (a) (→* () (Prompt-TagTop) (∪ a (¬ a))))]
  [goto (∀ (a) (case→ (→ Label ⊥) (→ (¬ a) a ⊥)))]
  [current-continuation
   (∀ (a)
      (case→ (→* () (Prompt-TagTop) (∪ a (¬ a)))
             (→ Label ⊥)
             (→ (¬ a) a ⊥)))])
(provide (rename-out [current-continuation cc]))
