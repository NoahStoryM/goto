#lang typed/racket/base

(require/typed/provide goto/private/goto
  [#:opaque Label label?]
  [label (→* () (Prompt-TagTop) Label)]
  [goto (→* (Label) ((U Label (→ Label))) Nothing)]
  [current-continuation (case→ (→ Label) (→* (Label) ((U Label (→ Label))) Nothing))])
(provide (rename-out [current-continuation cc]))
