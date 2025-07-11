#lang typed/racket/base/shallow

(define-type (Goto a) (∪ a (→ a Nothing)))
(define-type Label (→ Label Nothing))
(provide Goto Label)

(require/typed/provide goto
  [label (∀ (a) (→* () (Prompt-TagTop) (Goto a)))]
  [goto Label]
  [current-continuation (∀ (a) (case→ (→* () (Prompt-TagTop) (Goto a)) (→ Label Nothing)))])
(provide (rename-out [current-continuation cc]))
