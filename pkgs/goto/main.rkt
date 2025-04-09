#lang racket/base

(require racket/contract/base
         (only-in "private/goto.rkt" label?))
(require (contract-in "private/goto.rkt"
                      [label (->* () (continuation-prompt-tag?) label?)]
                      [goto (->* (label?) (label?) none/c)]
                      [current-continuation (parameter/c label?)]))
(provide label? label goto current-continuation (rename-out [current-continuation cc]))
