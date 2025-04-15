#lang racket/base

(require racket/contract/base)
(require (contract-in "no-check.rkt"
                      [label (->* () (continuation-prompt-tag?) any)]
                      [goto (->* (continuation?) (continuation?) none/c)]
                      [current-continuation (case-> (-> any)
                                                    (-> continuation? none/c)
                                                    (-> continuation? continuation? none/c))]))
(provide label goto current-continuation (rename-out [current-continuation cc]))
