#lang racket/base

(provide label? (rename-out [make-label label]) goto current-continuation)

(struct label (k))
(define (make-label [prompt-tag (default-continuation-prompt-tag)])
  (call/cc goto* prompt-tag))
(define current-continuation
  (case-λ
    [() (make-label)]
    [(l) (current-continuation l l)]
    [(l0 l1) ((label-k l0) l1)]))
(define (goto l0 [l1 l0]) (current-continuation l0 l1))
(define goto* (compose1 current-continuation label))
