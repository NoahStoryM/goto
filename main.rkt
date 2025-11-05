#lang typed/racket/base/optional

(define-type (¬ a) (→ a Nothing))
(define-type Label (¬ Label))
(provide ¬ Label)

(require/typed/provide "private/goto.rkt"
  [label (∀ (a) (→* () (Prompt-TagTop) (∪ a (¬ a))))]
  [goto Label]
  [current-continuation (∀ (a) (case→ (→* () (Prompt-TagTop) (∪ a (¬ a))) (→ Label Nothing)))])
(provide (rename-out [current-continuation cc]))
