#lang typed/racket/base

(require typed/racket/unsafe "private/types.rkt")
(unsafe-require/typed/provide goto/no-check
  [label (→* () (Prompt-TagTop) Label)]
  [goto (→* (Label) (Label) Nothing)]
  [current-continuation (case→ (→ Label) (→* (Label) (Label) Nothing))])
(provide Label (rename-out [current-continuation cc]))
