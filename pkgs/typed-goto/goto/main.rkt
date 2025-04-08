#lang typed/racket/base

(require typed/racket/unsafe "private/types.rkt")
(unsafe-require/typed/provide goto/no-check
  [label (∀ (a ...) (→* () (Prompt-TagTop) (→ a ... Nothing)))]
  [goto (→ Label Nothing)]
  [current-continuation (∀ (a ...) (case→ (→ (→ a ... Nothing)) (→ Label Nothing)))])
(provide Label (rename-out [current-continuation cc]))
