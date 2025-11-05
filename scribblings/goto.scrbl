#lang scribble/manual

@(require (for-label racket/base
                     racket/case
                     racket/contract/base
                     racket/function
                     racket/sequence
                     (only-in typed/racket/base define-type → Nothing)
                     data/queue
                     goto)
          "utils.rkt")

@title{Goto}
@defmodule[goto #:packages ("goto")]
@author[@author+email["Noah Ma" "noahstorym@gmail.com"]]

@section{Overview}

This package provides @racket[label] and @racket[goto] constructs that simulate
@deftech{jump} using @racket[call/cc].

@section{API Reference}

@defproc[(goto [k continuation?]) none/c]{
Sets current continuation.

@racketblock[
(define (goto k) (k k))
]
}

@defproc[(label [prompt-tag continuation-prompt-tag? (default-continuation-prompt-tag)]) continuation?]{
Gets current continuation.

@racketblock[
(define (label [prompt-tag (default-continuation-prompt-tag)])
  (call/cc goto prompt-tag))
]
}

@defproc*[([(current-continuation [prompt-tag continuation-prompt-tag? (default-continuation-prompt-tag)]) continuation?]
           [(current-continuation [k continuation?]) none/c])]{
@racketblock[
(define (current-continuation [v (default-continuation-prompt-tag)])
  (if (continuation-prompt-tag? v)
      (label v)
      (goto v)))
]
}

@defproc*[([(cc [prompt-tag continuation-prompt-tag? (default-continuation-prompt-tag)]) continuation?]
           [(cc [k continuation?]) none/c])]{
Is an alias for @racket[current-continuation].
}

@deftypeconstr[(¬ a)]{
@racketblock[
(define-type (¬ a) (→ a Nothing))
]
}

@deftype[Label]{
Is the fixed point of @racket[¬].

@racketblock[
(define-type Label (¬ Label))
]
}

@section{Examples}

@subsection{Loop}

@racketblock[
(let ([x 0])
  (define loop (label))
  (set! x (add1 x))
  (when (< x 7) (goto loop))
  (displayln x))
]

@subsection{Yin-Yang Puzzle}

@racketblock[
(let ([kn (current-continuation)])
  (display #\@)
  (let ([kn+1 (current-continuation)])
    (display #\*)
    (kn kn+1)))
]

@racketblock[
(let* ([k #f] [k0 (label)])
  (unless k (set! k k0) (goto k))
  (display #\@)
  (let* ([kn k] [kn+1 (label)])
    (when (eq? kn k) (set! k kn+1) (goto k))
    (display #\*)
    (goto kn)))
]

@racketblock[
(define (k0 kn)
  (display #\@)
  (define (kn+1 k)
    (display #\*)
    (kn k))
  (kn+1 kn+1))
(k0 k0)
]

@subsection{Call with Current Continuation}

@racketblock[
(define (call/cc proc)
  (define (dispatcher k . v*)
    (if k
        (proc (curry k #f))
        (apply values v*)))
  (call-with-values cc dispatcher))
]

@racketblock[
(define (call/cc proc)
  (define v* #f)
  (define l (label))
  (if v*
      (apply values v*)
      (proc (λ vs (set! v* vs) (goto l)))))
]

@subsection{Light-Weight Process}

@racketblock[
(let ([lwp-queue (make-queue)])
  (define (start)
    (when (non-empty-queue? lwp-queue)
      (goto (dequeue! lwp-queue))))
  (define (lwp-enqueue! break continue)
    (define first? #t)
    (define l (label))
    (case/eq first?
      [(#t)
       (set! first? #f)
       (enqueue! lwp-queue l)
       (break)]
      [(#f) (continue)]))
  (define (pause) (lwp-enqueue! start void))
  (define (lwp thk) (lwp-enqueue! void (λ () (thk) (start))))

  (lwp (λ () (goto (begin0 (label) (pause) (display #\h)))))
  (lwp (λ () (goto (begin0 (label) (pause) (display #\e)))))
  (lwp (λ () (goto (begin0 (label) (pause) (display #\y)))))
  (lwp (λ () (goto (begin0 (label) (pause) (display #\!)))))
  (lwp (λ () (goto (begin0 (label) (pause) (newline)    ))))
  (start))
]

@subsection{Ambiguous Operator}

@racketblock[
(let ([task* '()])
  (define (fail)
    (if (null? task*)
        (error "Amb tree exhausted")
        (goto (car task*))))
  (define (amb* . alt*)
    (define first? #t)
    (define task (label))
    (when (null? alt*) (fail))
    (when first?
      (set! first? #f)
      (set! task* (cons task task*)))
    (define alt (car alt*))
    (set! alt* (cdr alt*))
    (when (null? alt*)
      (set! task* (cdr task*)))
    (when (eq? alt amb*)
      (goto task))
    (alt))
  (define-syntax-rule (amb exp* ...) (amb* (λ () exp*) ...))

  (let ([w-1 (amb "the" "that" "a")]
        [w-2 (amb "frog" "elephant" "thing")]
        [w-3 (amb "walked" "treaded" "grows")]
        [w-4 (amb "slowly" "quickly")])
    (define (joins? left right)
      (equal?
       (string-ref left (sub1 (string-length left)))
       (string-ref right 0)))
    (unless (joins? w-1 w-2) (amb))
    (unless (joins? w-2 w-3) (amb))
    (unless (joins? w-3 w-4) (amb))
    (list w-1 w-2 w-3 w-4)))
]
