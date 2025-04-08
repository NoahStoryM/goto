#lang racket/base

(provide label goto current-continuation (rename-out [current-continuation cc]))

(define (label [prompt-tag (default-continuation-prompt-tag)])
  (call/cc values prompt-tag))
(define (goto k) (k k))
(define current-continuation
  (case-λ
    [() (let/cc k k)]
    [(k) (k k)]))
