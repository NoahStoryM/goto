#lang typed/racket/base

(require typed/rackunit)
(require "../main.rkt")

(displayln 'Start)

(check-pred continuation? goto)

(test-begin
  (define x 0)
  (define loop : Label (label))
  (set! x (add1 x))
  (when (< x 5) (goto loop))
  (check-eqv? x 5))

(test-begin
  (define n 5)
  (define result 1)
  (define loop : Label (label))
  (unless (zero? n)
    (set! result (* result n))
    (set! n (sub1 n))
    (goto loop))
  (check-eqv? result 120))
