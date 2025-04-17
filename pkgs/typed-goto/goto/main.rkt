#lang typed/racket/base

(require "private/types.rkt")
(provide Goto Label)

(require typed/racket/unsafe)
(unsafe-require/typed/provide goto/no-check
  [label (∀ (a ...) (→* () (Prompt-TagTop) (→ a ... Nothing)))]
  [goto (→ Label Nothing)]
  [current-continuation (∀ (a ...) (case→ (→ (→ a ... Nothing)) (→ Label Nothing)))])
(provide (rename-out [current-continuation cc]))
