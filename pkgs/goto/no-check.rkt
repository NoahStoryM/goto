#lang racket/base

(provide label goto current-continuation (rename-out [current-continuation cc]))

(define current-continuation
  (case-λ
    [() (let/cc k k)]
    [(k) (k k)]))
(define cc current-continuation)

(define (label [prompt-tag (default-continuation-prompt-tag)])
  (call/cc values prompt-tag))
(define goto (let/cc return (cc (call/cc return))))
