#lang typed/racket/base/optional

(define-type (¬ a) (→ a Nothing))
(define-type Label (¬ Label))
(provide ¬ Label)

(require/typed/provide "private/goto.rkt"
  [label (∀ (a) (→* () (Prompt-TagTop) (∪ a (¬ a))))]
  [goto (∀ (a) (case→ (→ Label Nothing) (→ (¬ a) a Nothing)))]
  [current-continuation
   (∀ (a)
      (case→ (→* () (Prompt-TagTop) (∪ a (¬ a)))
             (→ Label Nothing)
             (→ (¬ a) a Nothing)))])
(provide (rename-out [current-continuation cc]))
