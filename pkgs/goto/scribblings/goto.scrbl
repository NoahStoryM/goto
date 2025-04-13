#lang scribble/manual

@(require (for-label racket/base racket/contract/base data/queue goto))

@title{Goto}
@defmodule[goto       #:packages ("goto")]
@defmodule[typed/goto #:packages ("typed-goto") #:no-declare]
@author[@author+email["Noah Ma" "noahstorym@gmail.com"]]

@section{Overview}

This package provides @racket[label] and @racket[goto] constructs that simulate
@deftech{jump} using @racket[call/cc].

@section{API Reference}

@defproc[(goto [k continuation?] [l continuation? k]) none/c]{
@racketblock[
(define (goto k [l k]) (k l))
]
}

@defproc[(label [prompt-tag continuation-prompt-tag? (default-continuation-prompt-tag)]) continuation?]{
@racketblock[
(define (label [prompt-tag (default-continuation-prompt-tag)])
  (call/cc goto prompt-tag))
]
}

@defproc*[([(current-continuation) continuation?]
           [(current-continuation [k continuation?] [l continuation? k]) none/c])]{
@racketblock[
(define current-continuation
  (case-λ
    [() (label)]
    [(k) (goto k)]
    [(k l) (goto k l)]))
]
}

@defproc*[([(cc) continuation?]
           [(cc [k continuation?] [l continuation? k]) none/c])]{
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

@racketblock[
((begin0 (cc) (display #\@))
 (begin0 (cc) (display #\*)))
]

@subsection{Light-Weight Process}

@racketblock[
(require data/queue)
(let ([lwp-queue (make-queue)])
  (define (start)
    (when (non-empty-queue? lwp-queue)
      (goto (dequeue! lwp-queue))))
  (define (lwp-enqueue! break continue)
    (define first? #t)
    (define l (label))
    (cond
      [first?
       (set! first? #f)
       (enqueue! lwp-queue l)
       (break)]
      [else (continue)]))
  (define (pause) (lwp-enqueue! start void))
  (define (lwp thk) (lwp-enqueue! void (λ () (thk) (start))))
  (lwp (λ () (goto (begin0 (label) (pause) (display #\h)))))
  (lwp (λ () (goto (begin0 (label) (pause) (display #\e)))))
  (lwp (λ () (goto (begin0 (label) (pause) (display #\y)))))
  (lwp (λ () (goto (begin0 (label) (pause) (display #\!)))))
  (lwp (λ () (goto (begin0 (label) (pause) (newline)    ))))
  (start))
]
