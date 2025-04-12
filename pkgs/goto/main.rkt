#lang racket/base

(require racket/contract/base)
(require (contract-in "private/goto.rkt"
                      [label (->* () (continuation-prompt-tag?) continuation?)]
                      [goto (->* (continuation?) (continuation?) none/c)]
                      [current-continuation (case-> (-> continuation?)
                                                    (-> continuation? none/c)
                                                    (-> continuation? continuation? none/c))]))
(provide label goto current-continuation (rename-out [current-continuation cc]))
