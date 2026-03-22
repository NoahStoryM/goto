#lang scribble/manual

@(require (for-label racket/base
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

@defproc[(goto [k (-> any/c none/c)] [v any/c k]) none/c]{
Sets current continuation.

@racketblock[
(define (goto k [v k]) (k v))
]
}

@defproc[(label [prompt-tag continuation-prompt-tag? (default-continuation-prompt-tag)]) any/c]{
Gets current continuation.

@racketblock[
(define (label [prompt-tag (default-continuation-prompt-tag)])
  (call/cc values prompt-tag))
]
}

@defproc*[([(current-continuation [prompt-tag continuation-prompt-tag? (default-continuation-prompt-tag)]) any/c]
           [(current-continuation [k (-> any/c none/c)] [v any/c k]) none/c])]{
@racketblock[
(define current-continuation
  (case-λ
    [() (label)]
    [(v) (if (continuation-prompt-tag? v) (label v) (goto v))]
    [(k v) (goto k v)]))
]
}

@defproc*[([(cc [prompt-tag continuation-prompt-tag? (default-continuation-prompt-tag)]) any/c]
           [(cc [k (-> any/c none/c)] [v any/c k]) none/c])]{
Is an alias for @racket[current-continuation].
}


@deftype[⊥]{
Is an alias for @racket[Nothing].

@racketblock[
(define-type ⊥ Nothing)
]
}

@deftypeconstr[(¬ a)]{
@racketblock[
(define-type (¬ a) (→ a ⊥))
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
(let ([kn (label)])
  (display #\@)
  (let ([kn+1 (label)])
    (display #\*)
    (goto kn kn+1)))
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
(define (call/cc proc [prompt-tag (default-continuation-prompt-tag)])
  (define v* (label prompt-tag))
  (if (list? v*)
      (apply values v*)
      (proc (λ vs (goto v* vs)))))
]

@racketblock[
(define (call/cc proc [prompt-tag (default-continuation-prompt-tag)])
  (define v* #f)
  (define l (label prompt-tag))
  (if v*
      (apply values v*)
      (proc (λ vs (set! v* vs) (goto l)))))
]

@subsection{Light-Weight Process}

@racketblock[
(let ([lwp-queue (make-queue)])
  (define (lwp thk)
    (enqueue! lwp-queue thk))
  (define (start)
    (when (non-empty-queue? lwp-queue)
      ((dequeue! lwp-queue))))
  (define (pause)
    (define l (label))
    (when l
      (enqueue! lwp-queue (λ () (goto l #f)))
      (start)))

  (lwp (λ () (let f () (pause) (display #\h) (f))))
  (lwp (λ () (let f () (pause) (display #\e) (f))))
  (lwp (λ () (let f () (pause) (display #\y) (f))))
  (lwp (λ () (let f () (pause) (display #\!) (f))))
  (lwp (λ () (let f () (pause) (newline)     (f))))
  (start))
]

@subsection{Ambiguous Operator}

@racketblock[
(let ([task* '()])
  (define (fail)
    (if (null? task*)
        (error "Amb tree exhausted")
        (goto (car task*) #f)))
  (define (amb* . alt*)
    (define task (label))
    (when (null? alt*) (fail))
    (when task (set! task* (cons task task*)))
    (define alt (car alt*))
    (set! alt* (cdr alt*))
    (when (null? alt*) (set! task* (cdr task*)))
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
