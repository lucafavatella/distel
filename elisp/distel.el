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
\\[erl-find-source-under-point]		- Jump from a function call to its definition.
\\[erl-find-source-unwind]		- Jump back from a function definition (multi-level).
\\[erl-eval-expression]	- Evaluate an erlang expression from the minibuffer.
\\[fprof]	- Profile (with fprof) an expression from the minibuffer.
\\[edb-toggle-interpret]	- Toggle debug interpreting of the module.
\\[edb-toggle-breakpoint]	- Toggle a debugger breakpoint at the current line.
\\[edb-monitor]	- Popup the debugger's process monitor buffer.
\\[erl-ie-session]	- Create an interactive \"session\" buffer.
\\[erl-ie-copy-buffer-to-session]	- Create an interactive \"session\" buffer from current buffer.
\\[erl-ie-copy-region-to-session]	- Create an interactive \"session\" buffer from region.

Most commands that pop up new buffers will save your original window
configuration, so that you can restore it by pressing 'q'. Use
`describe-mode' (\\[describe-mode]) on any Distel buffer when you want
to know what commands are available. To get more information about a
particular command, use \"\\[describe-key]\" followed by the command's key
sequence. For general information about Emacs' online help, use
\"\\[help-for-help]\".
"
  nil
  nil
  '(("\C-c\C-di" . edb-toggle-interpret)
    ("\C-c\C-db" . edb-toggle-breakpoint)
    ("\C-c\C-dm" . edb-monitor)
    ("\C-c\C-d:" . erl-eval-expression)
    ("\C-c\C-dp" . fprof)
    ("\C-c\C-d." . erl-find-source-under-point)
    ("\C-c\C-d," . erl-find-source-unwind)
    ("\C-c\C-dl" . erl-process-list)

    ;; distel-ie keybindings:
    ("\C-c\C-ds" . erl-ie-show-session)
    ("\C-c\C-dc" . erl-ie-copy-buffer-to-session)
    ("\C-c\C-dr" . erl-ie-copy-region-to-session)
    ("\C-\M-x"   . erl-ie-evaluate)

    ;; Possibly "controversial" shorter keys
    ("\M-."      . erl-find-source-under-point)	; usually `find-tag'
    ("\M-,"      . erl-find-source-unwind) ; usually `tags-loop-continue'
    ))

;; Setup mode-line info for erlang-extended-mode
;;
;; NB: Would use the LIGHTER argument for define-minor-mode, but it's
;; not working portably: my copy of Emacs21 disagrees with emacs20 and
;; xemacs on whether it should be quoted.
(add-to-list 'minor-mode-alist
	     '(erlang-extended-mode
	       (" EXT" (edb-module-interpreted ":interpreted" ""))))

