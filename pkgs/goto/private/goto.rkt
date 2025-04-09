#lang racket/base

(provide label? (rename-out [make-label label]) goto current-continuation)

(struct label (k) #:property prop:procedure (struct-field-index k))
(define (make-label [prompt-tag (default-continuation-prompt-tag)])
  (call/cc goto* prompt-tag))
(define (label* . _) (make-label))

(define (goto l0 [l1 l0]) (current-continuation* l1) (l0 l1))
(define goto* (compose1 goto label))

(define current-continuation* (make-parameter #f #f 'current-continuation))
(define current-continuation (make-derived-parameter current-continuation* goto label*))
