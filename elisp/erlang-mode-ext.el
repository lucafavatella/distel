;;; erlang-mode-ext.el --- Extensions to erlang-mode based on Distel

;; Prerequisites
(require 'erl)
(require 'erlang)
(require 'erl-service)

;; Debugger
(require 'edb)

;; Extended feature key bindings (C-x C-d prefix)

(define-minor-mode erlang-extended-mode
  "Minor mode extending erlang-mode."
  nil
  " EXT"
  '(("\C-c\C-di" . edb-toggle-interpret)
    ("\C-c\C-db" . edb-toggle-breakpoint)
    ("\C-c\C-dm" . edb-monitor)
    ("\C-c\C-d:" . erl-eval-expression)
    ("\C-c\C-dp" . fprof)
    ("\C-c\C-d." . erl-find-source-under-point)
    ("\C-c\C-dl" . erl-process-list)))

(provide 'erlang-mode-ext)
