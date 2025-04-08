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

@defproc[(goto [k0 label?] [k1 label? k0]) none/c]{
@racketblock[
(define (goto k0 [k1 k0]) (k0 k1))
]
}

@defproc[(label [prompt-tag continuation-prompt-tag? (default-continuation-prompt-tag)]) label?]{
@racketblock[
(define (label) (call/cc goto))
]
}

@defproc*[([(current-continuation) label?]
           [(current-continuation [k0 label?] [k1 label? k0]) none/c])]{
@racketblock[
(define current-continuation
  (case-λ
    [() (label)]
    [(k) (goto k)]
    [(k0 k1) (goto k0 k1)]))
]
}

@defproc*[([(cc) label?]
           [(cc [k0 label?] [k1 label? k0]) none/c])]{
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
