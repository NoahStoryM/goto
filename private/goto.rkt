#lang racket/base

(provide goto label current-continuation)

(define (goto* name k [v k])
  (if (procedure? k)
      (raise-result-error name "none/c" (k v))
      (raise-argument-error name "(-> any/c none/c)" k)))

(define (goto k [v k]) (goto* 'goto k v))
(define (label [prompt-tag (default-continuation-prompt-tag)])
  (call/cc values prompt-tag))

(define current-continuation
  (case-λ
    [() (label)]
    [(v) (if (continuation-prompt-tag? v)
             (label v)
             (goto* 'current-continuation v))]
    [(k v) (goto* 'current-continuation k v)]))
