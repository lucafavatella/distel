;;; erl-service.el --- High-level calls to Erlang services.

;;; Commentary:
;;
;; This module implements Emacs commands - i.e. M-x'able key-bind'able
;; sort - for doing stuff with Erlang nodes.
;;
;; The general implementation strategy is to make RPCs to the "distel"
;; erlang module, which does most of the work for us.

(require 'erlang)

(defvar erl-nodename-cache nil
  "The name of the node most recently contacted, for reuse in future
commands. Using C-u to bypasses the cache.")

(defun erl-read-nodename ()
  "Read a node name, either from a cached variable or the minibuffer.
If a prefix argument is in effect, the cache in skipped."
  (if (and (not current-prefix-arg)
	   erl-nodename-cache)
      erl-nodename-cache
    (let ((name-string (read-string "Node: ")))
      (let ((name (intern (if (string-match "@" name-string)
			      name-string
			    (concat name-string
				    "@" (erl-determine-hostname))))))
	(when (derl-node-p name)
	  (setq erl-nodename-cache name))
	name))))

;; ------------------------------------------------------------
;; RPC

(defun erl-rpc (k kargs node m f a)
  "Call {M,F,A} on NODE and deliver the result to the function K.
The first argument to K is the result from the RPC, followed by the
elements of KARGS."
  (erl-spawn
    (erl-send-rpc node m f a)
    (erl-rpc-receive k kargs)))

(defun erl-send-rpc (node mod fun args)
  (let ((m 'distel)
	(f 'rpc_entry)
	(a (list mod fun args)))
    (erl-send (tuple 'rex node)
	      ;; {Who, {call, M, F, A, GroupLeader}}
	      (tuple erl-self (tuple 'call m f a erl-group-leader)))))
  
(defun erl-rpc-receive (k kargs)
  "Receive the reply to an `erl-rpc'."
  (erl-receive (k kargs)
      (([rex Reply] (apply k (cons reply kargs))))))

(defun erpc (node m f a)
  "Make an RPC to an erlang node."
  (interactive (list (erl-read-nodename)
		     (intern (read-string "Module: "))
		     (intern (read-string "Function: "))
		     (eval-minibuffer "Args: ")))
  (erl-rpc (lambda (result) (message "RPC result: %S" result))
	   nil
	   node
	   m f a))

;; ------------------------------------------------------------
;; Process List: summary of all processes on a node.

(defun erl-process-list (node)
  "Show a list of all processes running on NODE.
The listing is requested asynchronously, and popped up in a buffer
when ready."
  (interactive (list (erl-read-nodename)))
  (erl-rpc #'erl-show-process-list (list node)
	   node 'distel 'process_list '()))

(defun erl-show-process-list (reply node)
  (with-current-buffer (get-buffer-create (format "*plist %S*" node))
    (process-list-mode)
    (let ((buffer-read-only nil))
      (erase-buffer)
      (let ((header (tuple-elt reply 1))
	    (infos (tuple-elt reply 2)))
	(put-text-property 0 (length header) 'face 'bold header)
	(insert header)
	(mapc #'erl-insert-process-info infos))
      (goto-char (point-min))
      (next-line 1))
    (select-window (display-buffer (current-buffer)))))

(defun erl-insert-process-info (info)
  "Insert INFO into the buffer.
INFO is [PID SUMMARY-STRING]."
  (let ((pid (tuple-elt info 1))
	(text (tuple-elt info 2)))
    (put-text-property 0 (length text) 'erl-pid pid text)
    (insert text)))

;; Process list major mode

(defvar erl-viewed-pid nil
  "PID being viewed.")
(make-variable-buffer-local 'erl-viewed-pid)
(defvar erl-old-window-configuration nil
  "Window configuration to return to when viewing is finished.")
(make-variable-buffer-local 'erl-old-window-configuration)

(defun erl-quit-viewer (&optional bury)
  "Quit the current view and restore the old window config.
When BURY is non-nil, buries the buffer instead of killing it."
  (interactive)
  (let ((cfg erl-old-window-configuration))
    (if bury
	(bury-buffer)
      (kill-this-buffer))
    (set-window-configuration cfg)))

(defun erl-bury-viewer ()
  "Bury the current view and restore the old window config."
  (interactive)
  (erl-quit-viewer t))

(defvar process-list-mode-map nil
  "Keymap for Process List mode.")

(when (null process-list-mode-map)
  (setq process-list-mode-map (make-sparse-keymap))
  (define-key process-list-mode-map [?q] 'erl-quit-viewer)
  (define-key process-list-mode-map [return] 'erl-show-process-info)
  (define-key process-list-mode-map [(control m)] 'erl-show-process-info)
  (define-key process-list-mode-map [?i] 'erl-show-process-info-item)
  (define-key process-list-mode-map [?b] 'erl-show-process-backtrace)
  (define-key process-list-mode-map [?m] 'erl-show-process-messages))

(defun process-list-mode ()
  "Major mode for viewing Erlang process listings.

Available commands:

\\[erl-quit-viewer]	- Quit the process listing viewer, restoring old window config.
\\[erl-show-process-info]	- Show process_info for process at point.
\\[erl-show-process-info-item]	- Show a piece of process_info for process at point.
\\[erl-show-process-backtrace]	- Show a backtrace for the process at point.
\\[erl-show-process-messages]	- Show the message queue for the process at point."
  (interactive)
  (kill-all-local-variables)
  (use-local-map process-list-mode-map)
  (setq mode-name "Process List")
  (setq major-mode 'process-list-mode)
  (setq erl-old-window-configuration (current-window-configuration))
  (run-hooks 'process-list-mode-hook))

(defun erl-show-process-info ()
  "Show information about process at point in a summary buffer."
  (interactive)
  (let ((pid (get-text-property (point) 'erl-pid)))
    (if (null pid)
	(message "No process at point.")
      (erl-view-process pid))))

(defun erl-show-process-info-item (item)
  "Show a piece of information about process at point."
  (interactive (list (intern (read-string "Item: "))))
  (let ((pid (get-text-property (point) 'erl-pid)))
    (if (null pid)
	(message "No process at point.")
      (erl-spawn
	(erl-send-rpc (erl-pid-node pid)
		      'distel 'process_info_item (list pid item))
	(erl-receive (item pid)
	    (([rex [ok String]]
	      (display-message-or-view string "*pinfo item*"))
	     (Other
	      (message "Error from erlang side of process_info:\n  %S"
		       other))))))))

(defun display-message-or-view (msg bufname &optional select)
  "Like `display-buffer-or-message', but with `view-buffer-other-window'.
That is, if a buffer pops up it will be in view mode, and pressing q
will get rid of it.

Only uses the echo area for single-line messages - or more accurately,
messages without embedded newlines. They may still need to wrap or
truncate to fit on the screen."
  (if (string-match "\n.*[^\\s-]" msg)
      ;; Contains a newline with actual text after it, so display as a
      ;; buffer
      (with-current-buffer (get-buffer-create bufname)
	(erase-buffer)
	(insert msg)
	(let ((win (display-buffer (current-buffer))))
	  (when select (select-window win))))
    ;; Print only the part before the newline (if there is
    ;; one). Newlines in messages are displayed as "^J" in emacs20,
    ;; which is ugly
    (string-match "[^\r\n]*" msg)
    (message (match-string 0 msg))))

(defun erl-show-process-messages ()
  (interactive)
  (erl-show-process-info-item 'messages))
(defun erl-show-process-backtrace ()
  (interactive)
  (erl-show-process-info-item 'backtrace))

;; ------------------------------------------------------------
;; Single process viewer

(defun erl-view-process (pid)
  (erl-spawn
    (process-view-mode)
    (setq erl-old-window-configuration (current-window-configuration))
    (setq viewed-pid pid)
    (erl-send-rpc (erl-pid-node pid)
		  'distel 'process_summary_and_trace (list erl-self pid))
    (erl-rpc-receive #'erl-process-summary-init (list pid)))
  (message "Sent async query.."))

(defun erl-process-summary-init (summary pid)
  (rename-buffer (generate-new-buffer-name
		  (format "*pinfo %S on %S*"
			  (erl-pid-id pid) (erl-pid-node pid))))
  (erase-buffer)
  (insert summary)
  (goto-char (point-min))
  (select-window (display-buffer (current-buffer)))
  (erl-process-trace-loop))

(defvar process-view-mode-map nil
  "Keymap for Process View mode.")

(unless process-view-mode-map
  (setq process-view-mode-map (make-sparse-keymap))
  (define-key process-view-mode-map [?q] 'erl-quit-viewer))

(defun process-view-mode ()
  "Major mode for viewing an Erlang process."
  (interactive)
  (kill-all-local-variables)
  (use-local-map process-view-mode-map)
  (setq mode-name "Process View")
  (setq major-mode 'process-view)
  (run-hooks 'process-view-mode-hook))

(defun erl-process-trace-loop ()
  (erl-receive ()
      (([trace_msg Text]
	(goto-char (point-max))
	(insert text)))
    (erl-process-trace-loop)))

;; ---------------------------------------------------------------------
;; fprof
;; Profiler front-end.

(defvar fprof-entries nil
  "Alist of Tag -> Properties.
Tag is a symbol like foo:bar/2
Properties is an alist of:
  'text     -> String
  'callers  -> list of Tag
  'callees  -> list of Tag
  'beamfile -> String | undefined")

(defvar fprof-header nil
  "Header listing for fprof text entries.
This is received from the Erlang module.")

(defun fprof (node expr)
  "Profile a function and summarise the results."
  (interactive (list (erl-read-nodename)
		     (erl-add-terminator (read-string "Expression: "))))
  (erl-spawn
    (erl-send-rpc node 'distel 'fprof (list expr))
    (message "Waiting for fprof reply...")
    (erl-receive ()
	(([rex [ok Preamble Header Entries]]
	  (message "Got fprof reply, drawing...")
	  (fprof-display preamble header entries))
	 (Other (message "Unexpected reply: %S" other))))))

(defun fprof-display (preamble header entries)
  "Display profiler results in the *fprof* buffer."
  (setq fprof-entries '())
  (setq fprof-header header)
  (with-current-buffer (get-buffer-create "*fprof*")
    (use-local-map (make-sparse-keymap))
    (define-key (current-local-map) [return] 'fprof-show-detail)
    (define-key (current-local-map) [(control m)] 'fprof-show-detail)
    (define-key (current-local-map) [?f] 'fprof-find-source)
    (define-key (current-local-map) [?q] 'kill-this-buffer)
    (setq tab-width 10)
    (erase-buffer)
    (insert preamble)
    (insert fprof-header)
    (mapc #'fprof-add-entry entries)
    (goto-char (point-min))
    (select-window (display-buffer (current-buffer)))))

(defun fprof-add-entry (entry)
  "Add a profiled function entry."
  (mcase entry
    ([process Title Info-List]
     (insert "\n")
     (insert title "\n")
     (dolist (info info-list)
       (insert "  " info "\n"))
     (insert "\n"))
    ([tracepoint Tag MFA Text Callers Callees Beamfile]
     (push `(,tag . ((text 	. ,text)
		     (mfa 	. ,mfa)
		     (callers 	. ,callers)
		     (callees 	. ,callees)
		     (beamfile 	. ,beamfile)))
	   fprof-entries)
     (fprof-insert text tag))))

(defun fprof-insert (text tag)
  (put-text-property 0 (length text) 'fprof-tag tag text)
  (insert text))

(defun fprof-show-detail ()
  "Show more detail about the profiled function at point.
The extra detail is a list of callers and callees, showing how much
time the function spent while called from each caller, and how much
time it spent in subfunctions."
  (interactive)
  (let* ((tag     (fprof-tag-at-point))
	 (props   (cdr (assq tag fprof-entries)))
	 (text    (cdr (assq 'text    props)))
	 (callers (cdr (assq 'callers props)))
	 (callees (cdr (assq 'callees props)))
	 (buf     (get-buffer-create "*fprof detail*")))
    (with-current-buffer buf
      (erase-buffer)
      (insert fprof-header)
      (insert text "\n")
      (insert "Callers:\n")
      (mapc #'fprof-insert-by-tag callers)
      (insert "\n")
      (insert "Callees:\n")
      (mapc #'fprof-insert-by-tag callees)
      (goto-char (point-min)))
    (display-buffer buf)))

(defun fprof-insert-by-tag (tag)
  (let ((text (fprof-lookup tag 'text)))
    (put-text-property 0 (length text) 'fprof-tag tag text)
    (insert text)))

(defun fprof-find-source ()
  (interactive)
  (let ((beamfile (fprof-lookup (fprof-tag-at-point) 'beamfile)))
    (if (eq beamfile 'undefined)
	(message "Don't know where that's implemented.")
      (let* ((src (fprof-sourcefile beamfile))
	     (mfa (fprof-lookup (fprof-tag-at-point) 'mfa))
	     (arity (caddr mfa))
	     (orig-window (selected-window)))
	(when src
	  (with-current-buffer (find-file-other-window src)
	    (goto-char (point-min))
	    ;; Find the right function/arity
	    (let (found)
	      (while (and (not found)
			  (re-search-forward (concat "^" (symbol-name (cadr mfa)))))
		(beginning-of-line)
		(if (eq (erlang-get-function-arity) arity)
		    (setq found t)
		  (forward-line)))
	      (if found
		  (recenter 5))))
	  (select-window orig-window))))))

(defun fprof-tag-at-point ()
  (or (get-text-property (point) 'fprof-tag)
      (error "No function tag at point.")))

(defun fprof-lookup (tag property)
  (cdr (assq property (cdr (assq tag fprof-entries)))))

(defun fprof-sourcefile (beamfile)
  (let ((string beamfile))
    (when (string-match "ebin" string)
      (setq string (replace-match "src" t t string)))
    (if (null (string-match "beam" string))
	nil
      (setq string (replace-match "erl" t t string))
      (if (file-exists-p string)
	  string
	nil))))

;;

(defun erl-eval-expression (node string)
  (interactive (list (erl-read-nodename)
		     (erl-add-terminator (read-string "Expression: "))))
  (erl-spawn
    (erl-send-rpc node
		  'distel
		  'eval_expression
		  (list string))
    (erl-receive ()
	(([rex [ok String]]
	  (display-message-or-view string "*Expression Result*"))
	 ([rex [error Reason]]
	  (message "Error: %S" reason))
	 (Other
	  (message "Unexpected: %S" other))))))

(defun erl-add-terminator (s)
  "Make sure S terminates with a dot (.)"
  (if (string-match "\\.\\s *$" s)
      s
    (concat s ".")))

;; ------------------------------------------------------------
;; Find the source for a module

(defvar erl-find-history-ring (make-ring 20)
  "History ring tracing for following functions to their definitions.")

(defun erl-find-source-under-point (node)
  "Goto the source code that defines the function being called at point.
For remote calls, contacts an Erlang node to determine which file to
look in, with the following algorithm:

  Find the directory of the module's beam file (loading it if necessary).
  Look for the source file in:
    Same directory as the beam file
    Again with /ebin/ replaced with /src/
    Again with /ebin/ replaced with /erl/
    Directory where source file was originally compiled

  Otherwise, report that the file can't be found."
  (interactive (list (erl-read-nodename)))
  (let ((mfa (erl-get-call-mfa)))
    (when (null mfa)
      (error "Couldn't determine MFA of call"))
    (apply #'erl-find-source (cons node mfa))))

(defun erl-find-source-unwind ()
  "Unwind back from uses of `erl-find-source-under-point'."
  (interactive)
  (unless (ring-empty-p erl-find-history-ring)
    (let* ((marker (ring-remove erl-find-history-ring))
	   (buffer (marker-buffer marker)))
      (if (buffer-live-p buffer)
	  (progn (switch-to-buffer buffer)
		 (goto-char (marker-position marker)))
	;; If this buffer was deleted, recurse to try the next one
	(erl-find-source-unwind)))))

(defun erl-get-call-mfa ()
  "Return (MODULE FUNCTION ARITY) of the function call under point.
If there is no function call under point, returns nil.
ARITY is returned NIL if it cannot be determined."
  (save-excursion
    (erl-goto-end-of-call-name)
    (let ((arity (erl-get-arity)))
      (let ((mf (erlang-get-function-under-point)))
	(unless (null mf)
	  (let ((module (intern (or (car mf)
				    (erlang-get-module))))
		(function (intern (cadr mf))))
	    (list module function arity)))))))

(defun erl-goto-end-of-call-name ()
  "Go to the end of the function or module:function part of a function call."
  ;; We basically just want to do forward-sexp iff we're not already
  ;; in the right place
  (unless (member (char-syntax (char-after (point)))
		  '(?w ?_))
    (backward-sexp))
  (forward-sexp)
  ;; Special case handling: On some emacs installations (Tobbe's
  ;; machine), the (forward-sexp) won't skip over the : in a remote
  ;; function call. This is a workaround for that. The issue seems to
  ;; be that the emacs considers : to be punctuation (syntax class
  ;; '.'), whereas my emacs calls it a symbol separator (syntax class
  ;; '_'). FIXME.
  (when (eq (char-after) ?:)
    (forward-sexp)))

(defun erl-get-arity ()
  "Get the number of arguments in a function call.
Should be called with point directly before the opening `('."
  ;; Adapted from erlang-get-function-arity.
  (save-excursion
    (cond ((looking-at "/")
	   ;; form is /<n>, like the /2 in foo:bar/2
	   (forward-char)
	   (let ((start (point)))
	     (re-search-forward "\\([^0-9]\\|$\\)" nil t)
	     (backward-char)
	     (ignore-errors
	       (car (read-from-string (buffer-substring start (point)))))))
	  ((looking-at "[\n\r ]*(")
	   (goto-char (match-end 0))
	   (condition-case nil
	       (let ((res 0)
		     (cont t))
		 (while cont
		   (cond ((eobp)
			  (setq res nil)
			  (setq cont nil))
			 ((looking-at "\\s *)")
			  (setq cont nil))
			 ((looking-at "\\s *\\($\\|%\\)")
			  (forward-line 1))
			 ((looking-at "\\s *,")
			  (incf res)
			  (goto-char (match-end 0)))
			 (t
			  (when (zerop res)
			    (incf res))
			  (forward-sexp 1))))
		 res)
	     (error nil))))))

(defun erl-find-source (node module &optional function arity)
  "Find the source code for MODULE in a buffer, loading it if necessary.
When FUNCTION is specified, the point is moved to its start."
  ;; Add us to the history list
  (ring-insert-at-beginning erl-find-history-ring
			    (copy-marker (point-marker)))
  (if (eq module (intern (erlang-get-module)))
      (when function
	(erl-search-function function arity))
      (erl-spawn
	  (erl-send-rpc node 'distel 'find_source (list module))
	  (erl-receive (function arity)
	      (([rex [ok Path]]
		(find-file path)
		(when function
		  (erl-search-function function arity)))
	       ([rex [error Reason]]
		;; Remove the history marker, since we didn't go anywhere
		(ring-remove erl-find-history-ring)
		(message "Error: %s" reason)))))))

(defun erl-search-function (function arity)
  "Goto the definition of FUNCTION/ARITY in the current buffer."
  (let ((origin (point))
	(str (concat "\n" (symbol-name function) "("))
	(searching t))
    (goto-char (point-min))
    (while searching
      (cond ((search-forward str nil t)
	     (backward-char)
	     (when (or (null arity)
		       (eq (erl-get-arity) arity))
	       (beginning-of-line)
	       (setq searching nil)))
	    (t
	     (setq searching nil)
	     (goto-char origin)
	     (if arity
		 (message "Couldn't find function %S/%S" function arity)
	       (message "Couldn't find function %S" function)))))))

(defun erl-read-symbol-or-nil (prompt)
  "Read a symbol, or NIL on empty input."
  (let ((s (read-string prompt)))
    (if (string= s "")
	nil
      (intern s))))

(provide 'erl-service)

