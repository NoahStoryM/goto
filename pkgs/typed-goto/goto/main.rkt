#lang typed/racket/base/shallow

(define-type (Goto a) (∪ a (→ a Nothing)))
(define-type Label (→ Label Nothing))
(provide Goto Label)

(require/typed/provide goto/no-check
  [label (∀ (a ...) (→* () (Prompt-TagTop) (→ a ... Nothing)))]
  [goto Label]
  [current-continuation (∀ (a ...) (case→ (→ (→ a ... Nothing)) (→ Label Nothing)))])
(provide (rename-out [current-continuation cc]))
