#lang typed/racket/base/optional

(require "private/types.rkt")
(provide Goto Label)

(require/typed/provide goto/no-check
  [label (∀ (a ...) (→* () (Prompt-TagTop) (→ a ... Nothing)))]
  [goto Label]
  [current-continuation (∀ (a ...) (case→ (→ (→ a ... Nothing)) (→ Label Nothing)))])
(provide (rename-out [current-continuation cc]))
