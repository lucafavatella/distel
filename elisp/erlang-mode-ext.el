;;; erlang-mode-ext.el --- Extensions to erlang-mode based on Distel

;; Prerequisites
(require 'erl)
(require 'erlang)
(require 'erl-service)

;; Debugger
(require 'edb)

;; Extended feature key bindings (C-x C-d prefix)

(define-minor-mode erlang-extended-mode
  "Extensions to erlang-mode for communicating with a running Erlang node.

\\[erl-process-list]	- List all Erlang processes (\"pman\").
\\[erl-find-source-under-point]	- Jump to the definition of a function call (dynamic TAGS).
\\[erl-eval-expression]	- Evaluate an erlang expression from the minibuffer.
\\[fprof]	- Profile (with fprof) an expression from the minibuffer.
\\[edb-toggle-interpret]	- Toggle debug interpreting of the module.
\\[edb-toggle-breakpoint]	- Toggle a debugger breakpoint at the current line.
\\[edb-monitor]	- Popup the debugger's process monitor buffer.
"
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
