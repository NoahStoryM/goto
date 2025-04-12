#lang racket/base

(provide label goto current-continuation)

(define (label [prompt-tag (default-continuation-prompt-tag)])
  (call/cc values prompt-tag))
(define (goto k [l k]) (k l))
(define current-continuation
  (case-λ
    [() (let/cc k k)]
    [(k) (k k)]
    [(k l) (k l)]))
