#lang scribble/manual

@(require (for-label racket/base
                     racket/contract/base
                     racket/sequence
                     data/queue
                     goto))

@title{Goto}
@defmodule[goto       #:packages ("goto")]
@defmodule[typed/goto #:packages ("typed-goto") #:no-declare]
@author[@author+email["Noah Ma" "noahstorym@gmail.com"]]

@section{Overview}

This package provides @racket[label] and @racket[goto] constructs that simulate
@deftech{jump} using @racket[call/cc].

@section{API Reference}

@defproc[(goto [k continuation?]) none/c]{
@racketblock[
(define (goto k) (k k))
]
}

@defproc[(label [prompt-tag continuation-prompt-tag? (default-continuation-prompt-tag)]) continuation?]{
@racketblock[
(define (label [prompt-tag (default-continuation-prompt-tag)])
  (call/cc goto prompt-tag))
]
}

@defproc*[([(current-continuation) continuation?]
           [(current-continuation [k continuation?]) none/c])]{
@racketblock[
(define current-continuation
  (case-λ
    [() (label)]
    [(k) (goto k)]))
]
}

@defproc*[([(cc) continuation?]
           [(cc [k continuation?]) none/c])]{
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
(let ([yin (current-continuation)])
  (display #\@)
  (let ([yang (current-continuation)])
    (display #\*)
    (yin yang)))
]

@racketblock[
((begin0 (cc) (display #\@))
 (begin0 (cc) (display #\*)))
]

@subsection{Call with Current Continuation}

@racketblock[
(define (call/cc proc)
  (define first? #t)
  (define l (call-with-values cc list))
  (cond
    [first? (set! first? #f) (proc (car l))]
    [else (apply values l)]))
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

@subsection{Ambiguous Operator}

@racketblock[
(define task* '())
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
  (list w-1 w-2 w-3 w-4))
]
