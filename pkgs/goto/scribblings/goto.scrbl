#lang scribble/manual

@(require (for-label racket/base racket/contract/base data/queue goto))

@title{Goto}
@defmodule[goto       #:packages ("goto")]
@defmodule[typed/goto #:packages ("typed-goto") #:no-declare]
@author[@author+email["Noah Ma" "noahstorym@gmail.com"]]

@section{Overview}

This package provides @racket[label] and @racket[goto] constructs that simulate
@deftech{goto} using @racket[call/cc].

@section{API Reference}

@defproc[(label? [v any/c]) boolean?]

@defproc[(goto [l label?] [p (or/c label? (-> label?)) l]) none/c]{
@racketblock[
(define (goto l [p l])
  (if (label? p)
      (l p)
      (call-in-continuation l p)))
]
}

@defproc[(label [prompt-tag continuation-prompt-tag? (default-continuation-prompt-tag)]) label?]{
@racketblock[
(define (label [prompt-tag (default-continuation-prompt-tag)])
  (call/cc goto prompt-tag))
]
}

@defproc*[([(current-continuation) label?]
           [(current-continuation [l label?] [p (or/c label? (-> label?)) l]) none/c])]{
@racketblock[
(define current-continuation
  (case-λ
    [() (label)]
    [(l) (goto l)]
    [(l p) (goto l p)]))
]
}

@defproc*[([(cc) label?]
           [(cc [l label?] [p (or/c label? (-> label?)) l]) none/c])]{
The @racket[cc] binding is an alias for @racket[current-continuation].
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

@subsection{Light-Weight Process}

@racketblock[
(require data/queue)
(let ([lwp-run? #f] [lwp-queue (make-queue)])
  (define (start)
    (set! lwp-run? #t)
    (when (non-empty-queue? lwp-queue)
      (goto (dequeue! lwp-queue))))
  (define (pause)
    (define first? #t)
    (define l (label))
    (when first?
      (set! first? #f)
      (enqueue! lwp-queue l)
      (start)))
  (define-syntax-rule (lwp exp* ...)
    (let ([l (label)])
      (cond
        [lwp-run? exp* ... (start)]
        [else (enqueue! lwp-queue l)])))
  (lwp (let ([l (label)]) (pause) (display #\h) (goto l)))
  (lwp (let ([l (label)]) (pause) (display #\e) (goto l)))
  (lwp (let ([l (label)]) (pause) (display #\y) (goto l)))
  (lwp (let ([l (label)]) (pause) (display #\!) (goto l)))
  (lwp (let ([l (label)]) (pause) (newline)     (goto l)))
  (start))
]

@subsection{Yin-Yang Puzzle}

@racketblock[
(let ([yin (label)])
  (display #\@)
  (let ([yang (label)])
    (display #\*)
    (goto yin yang)))
]
