;;; distel.el --- Top-level of distel package, loads all subparts

;; Prerequisites
(require 'erlang)
(require 'easy-mmode)

(provide 'distel)

(defconst distel-version "3.1")

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
\\[erl-reload-module]	- Reload an Erlang module.
\\[fprof]	- Profile (with fprof) an expression from the minibuffer.
\\[fprof-analyse]	- View profiler results from an \"fprof:analyse\" file.
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
    ("\C-c\C-dL" . erl-reload-module)
    ("\C-c\C-dp" . fprof)
    ("\C-c\C-dP" . fprof-analyse)
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
    ("\M-*"      . erl-find-source-unwind) ; usually `tags-loop-continue'
    ))

;; Setup mode-line info for erlang-extended-mode
;;
;; NB: Would use the LIGHTER argument for define-minor-mode, but it's
;; not working portably: my copy of Emacs21 disagrees with emacs20 and
;; xemacs on whether it should be quoted.
(add-to-list 'minor-mode-alist
	     '(erlang-extended-mode
	       (" EXT" (edb-module-interpreted ":interpreted" ""))))

;; Bug reportage

(defvar distel-bugs-address "distel-hackers@lists.sourceforge.net"
  "Email address to send distel bugs to.")

(defun report-distel-problem (summary)
  "Report a bug to the distel-hackers mailing list."
  (interactive (list (read-string "One-line summary: ")))
  (compose-mail distel-bugs-address
		(format "PROBLEM: %s" summary))
  (insert (propertize "\
;; Distel bug report form.
;;
;; This is an email message buffer for you to fill in information
;; about your problem. When finished, you can enter \"C-c C-c\" to
;; send the report to the distel-hackers mailing list - or update the
;; 'To: ' header line to send it somewhere else.
;;
;; Please describe the problem in detail in this blank space:

"
		      'face font-lock-comment-face))
  (save-excursion
    (insert (propertize "\


;; Below is some automatically-gathered debug information. Please make
;; sure it doesn't contain any secrets that you don't want to send. If
;; you decide to censor it or have any other special notes, please
;; describe them here:



"
			'face font-lock-comment-face))
    (insert "[ ---- Automatically gathered trace information ---- ]\n\n")
    (insert (format "Emacs node name: %S\n\n" erl-node-name))
    (insert (format "Node of most recent command: %S\n\n" erl-nodename-cache))
    (insert "Recent *Messages*:\n")
    (distel-indented-insert (distel-last-lines "*Messages*" 15) 2)
    (insert "\n\n")
    (when erl-nodename-cache
      (insert (format "Recent interactions with %S:\n" erl-nodename-cache))
      (distel-indented-insert (distel-last-lines
			       (format "*trace %S*" erl-nodename-cache) 50)
			      2))
    (insert "\n\nThe End.\n\n")))

(defun distel-last-lines (buffer n)
  (with-current-buffer buffer
    (save-excursion
      (goto-char (point-max))
      (forward-line (- n))
      (buffer-substring (point) (point-max)))))

(defun distel-indented-insert (string level)
  (let ((pos (point)))
    (insert string)
    (indent-rigidly pos (point) level)))

(defvar distel-tags-compliant '()
  "Tags compliant, i.e. let M-. ask for confirmation.")
