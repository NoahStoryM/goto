#lang racket/base

(require racket/contract/base)
(require (contract-in "no-check.rkt"
                      [label (->* () (continuation-prompt-tag?) any)]
                      (rename cc goto (-> continuation? none/c))
                      [current-continuation (case-> (-> any)
                                                    (-> continuation? none/c))]))
(define goto* (let/cc return (goto (call/cc return))))
(provide label current-continuation (rename-out [current-continuation cc] [goto* goto]))
