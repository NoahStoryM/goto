#lang scribble/manual

@(require (for-label racket/base racket/contract/base goto))

@title{Goto}
@defmodule[goto       #:packages ("goto")]
@defmodule[typed/goto #:packages ("typed-goto") #:no-declare]
@author[@author+email["Noah Ma" "noahstorym@gmail.com"]]

@section{Overview}

This package provides @racket[label] and @racket[goto] constructs that simulate
@deftech{goto} using @racket[call/cc].

@section{API Reference}

@defproc[(label? [v any/c]) boolean?]

@defproc[(goto [l0 label?] [l1 label? l0]) none/c]{
@racketblock[
(define (goto l0 [l1 l0]) (l0 l1))
]
}

@defproc[(label [prompt-tag continuation-prompt-tag? (default-continuation-prompt-tag)]) label?]{
@racketblock[
(define (label) (call/cc goto))
]
}

@defproc*[([(current-continuation) label?]
           [(current-continuation [l0 label?] [l1 label? l0]) none/c])]{
@racketblock[
(define current-continuation
  (case-λ
    [() (label)]
    [(l) (goto l)]
    [(l0 l1) (goto l0 l1)]))
]
}

@defproc*[([(cc) label?]
           [(cc [l0 label?] [l1 label? l0]) none/c])]{
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

@subsection{Yin-Yang Puzzle}

@racketblock[
(let ([yin (label)])
  (display #\@)
  (let ([yang (label)])
    (display #\*)
    (goto yin yang)))
]
