;;; distel.el --- Top-level of distel package, loads all subparts

;; Prerequisites
(require 'erlang)
(require 'easy-mmode)

(provide 'distel)

;; Compatibility with XEmacs
(unless (fboundp 'define-minor-mode)
  (defalias 'define-minor-mode 'easy-mmode-define-minor-mode))

;; Distel modules

(require 'erl)
(require 'erl-service)
(require 'edb)

(require 'distel-ie)

(defun distel-erlang-mode-hook ()
  "Function to enable the Distel extensions to Erlang mode.
You can add this to erlang-mode-hook with:
  (add-hook 'erlang-mode-hook 'distel-erlang-mode-hook)"
  (erlang-extended-mode t))

;; Extended feature key bindings (C-c C-d prefix)

(define-minor-mode erlang-extended-mode
  "Extensions to erlang-mode for communicating with a running Erlang node.

These commands generally communicate with an Erlang node. The first
time you use one, you will be prompted for the name of the node to
use. This name will be cached for future commands. To override the
cache, give a prefix argument with C-u before using the command.
\\<erlang-extended-mode-map>
\\[erl-process-list]	- List all Erlang processes (\"pman\").
\\[erl-find-source-under-point]		- Jump to the definition of a function call (dynamic TAGS).
\\[erl-find-source-unwind]		- Jump back from a definition.
\\[erl-eval-expression]	- Evaluate an erlang expression from the minibuffer.
\\[fprof]	- Profile (with fprof) an expression from the minibuffer.
\\[edb-toggle-interpret]	- Toggle debug interpreting of the module.
\\[edb-toggle-breakpoint]	- Toggle a debugger breakpoint at the current line.
\\[edb-monitor]	- Popup the debugger's process monitor buffer.
\\[erl-ie-session]	- Create an interactive \"session\" buffer.
\\[erl-ie-copy-buffer-to-session]	- Create an interactive \"session\" buffer from current buffer.
\\[erl-ie-copy-region-to-session]	- Create an interactive \"session\" buffer from region.
"
  nil
  " EXT"
  '(("\C-c\C-di" . edb-toggle-interpret)
    ("\C-c\C-db" . edb-toggle-breakpoint)
    ("\C-c\C-dm" . edb-monitor)
    ("\C-c\C-d:" . erl-eval-expression)
    ("\C-c\C-dp" . fprof)
    ("\C-c\C-d." . erl-find-source-under-point)
    ("\C-c\C-d," . erl-find-source-unwind)
    ("\C-c\C-dl" . erl-process-list)

    ;; distel-ie keybindings:
    ("\C-c\C-ds" . erl-ie-session)
    ("\C-c\C-dc" . erl-ie-copy-buffer-to-session)
    ("\C-c\C-dr" . erl-ie-copy-region-to-session)
    ("\C-\M-x"   . erl-ie-evaluate)

    ;; Possibly "controversial" shorter keys
    ("\M-."      . erl-find-source-under-point)	; usually `find-tag'
    ("\M-,"      . erl-find-source-unwind) ; usually `tags-loop-continue'
    ))
