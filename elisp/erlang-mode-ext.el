;;; erlang-mode-ext.el --- Extensions to erlang-mode based on Distel

;; Prerequisites
(require 'erl)
(require 'erlang)
(require 'erl-service)

;; Debugger
(require 'edb)

;; Extended feature key bindings (C-x C-e prefix)
(define-key erlang-mode-map "\C-c\C-e:" 'erl-eval-expression)
(define-key erlang-mode-map "\C-c\C-ep" 'fprof)
(define-key erlang-mode-map "\C-c\C-ef" 'erl-find-source)
(define-key erlang-mode-map "\C-c\C-el" 'erl-process-list)

(provide 'erlang-mode-ext)
