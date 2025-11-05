#lang racket/base

(provide goto label current-continuation)

(define (goto* name k)
  (if (continuation? k)
      (raise-result-error name "none/c" (k k))
      (raise-argument-error name "continuation?" k)))

(define goto (let/cc return (goto* 'goto (call/cc return))))
(define (label [prompt-tag (default-continuation-prompt-tag)])
  (call/cc values prompt-tag))

(define (current-continuation [v (default-continuation-prompt-tag)])
  (if (continuation-prompt-tag? v)
      (label v)
      (goto* 'current-continuation v)))
