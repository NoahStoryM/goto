#lang racket/base

(provide label? (rename-out [make-label label]) goto current-continuation)

(struct label (k) #:property prop:procedure (struct-field-index k))
(define (make-label [prompt-tag (default-continuation-prompt-tag)])
  (call/cc goto* prompt-tag))
(define current-continuation
  (case-λ
    [() (make-label)]
    [(l) (current-continuation l l)]
    [(l p) (if (label? p) (l p) (call-in-continuation (label-k l) p))]))
(define (goto l [p l]) (current-continuation l p))
(define goto* (compose1 current-continuation label))
