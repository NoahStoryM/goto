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

@subsection{Ambiguous Operator}

@racketblock[
(require racket/sequence data/queue)

(define amb-tasks (make-queue))
(define (fail)
  (if (queue-empty? amb-tasks)
      (error "Amb tree exhausted")
      (goto (sequence-ref amb-tasks 0))))
(define (-amb alt*)
  (if (zero? (vector-length alt*))
      (fail)
      (let* ([pos #f] [task (label)])
        (cond
          [(integer? pos)
           (define alt (vector-ref alt* pos))
           (vector-set! alt* pos #f)
           (set! pos (add1 pos))
           (when (= pos (vector-length alt*))
             (dequeue! amb-tasks)
             (set! alt* #f)
             (set! pos #t))
           (alt)]
          [(not pos)
           (set! pos 0)
           (enqueue! amb-tasks task)
           (goto (sequence-ref amb-tasks 0))]
          [else
           (fail)]))))
(define-syntax-rule (amb exp* ...) (-amb (vector (λ () exp*) ...)))

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
