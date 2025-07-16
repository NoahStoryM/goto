#lang typed/racket/base/shallow

(define-type (¬ a) (→ a Nothing))
(define-type Label (¬ Label))
(provide ¬ Label)

(require/typed/provide goto
  [label (∀ (a) (→* () (Prompt-TagTop) (∪ a (¬ a))))]
  [goto Label]
  [current-continuation (∀ (a) (case→ (→* () (Prompt-TagTop) (∪ a (¬ a))) (→ Label Nothing)))])
(provide (rename-out [current-continuation cc]))
