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
    (let ((name (intern (read-string "Node: "))))
      (when (derl-node-p name)
	(setq erl-nodename-cache name))
      name)))

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
      (([tuple rex Reply] (apply k (cons reply kargs))))))

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
      (let ((header (elt reply 1))
	    (infos (elt reply 2)))
	(put-text-property 0 (length header) 'face 'bold header)
	(insert header)
	(mapc #'erl-insert-process-info infos))
      (goto-char (point-min))
      (next-line 1))
    (select-window (display-buffer (current-buffer)))))

(defun erl-insert-process-info (info)
  "Insert INFO into the buffer.
INFO is [tuple PID SUMMARY-STRING]."
  (let ((pid (elt info 1))
	(text (elt info 2)))
    (put-text-property 0 (length text) 'erl-pid pid text)
    (insert text)))

;; Process list major mode

(defvar erl-viewed-pid nil
  "PID being viewed.")
(make-variable-buffer-local 'erl-viewed-pid)
(defvar erl-old-window-configuration nil
  "Window configuration to return to when viewing is finished.")
(make-variable-buffer-local 'erl-old-window-configuration)

(defun erl-quit-viewer ()
  "Quit the current view and restore the original window
  configuration."
  (interactive)
  (let ((cfg erl-old-window-configuration))
    (kill-this-buffer)
    (set-window-configuration cfg)))

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
  "Major mode for viewing Erlang process listings."
  (interactive)
  (kill-all-local-variables)
  (use-local-map process-list-mode-map)
  (setq mode-name "Process List")
  (setq major-mode 'process-list)
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
	    (([tuple rex [tuple ok String]]
	      (display-message-or-view string "*pinfo item*"))
	     (Other
	      (message "Error from erlang side of process_info:\n  %S"
		       other))))))))

(defun display-message-or-view (msg bufname)
  "Like `display-buffer-or-message', but with `view-buffer-other-window'.
That is, if a buffer pops up it will be in view mode, and pressing q
will get rid of it."
  (let ((buf (save-window-excursion
	       (let ((res (display-message-or-buffer msg)))
		 (if (windowp res)
		     (window-buffer res)
		   nil)))))
    (when buf
      ;; Kill old buffer
      (when (get-buffer bufname)
	(kill-buffer bufname))
      (with-current-buffer buf
	(rename-buffer bufname))
      (let ((win (selected-window)))
	(view-buffer-other-window buf)
	(select-window win)))))

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
      (([tuple trace_msg Text]
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
	(([tuple rex [tuple ok Preamble Header Entries]]
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
    (erase-buffer)
    (insert preamble)
    (insert fprof-header)
    (mapc #'fprof-add-entry entries)
    (goto-char (point-min))
    (select-window (display-buffer (current-buffer)))))

(defun fprof-add-entry (entry)
  "Add a profiled function entry."
  (pmatch [tuple Tag MFA Text Callers Callees Beamfile] entry
    (push `(,tag . ((text 	. ,text)
		    (mfa 	. ,mfa)
		    (callers 	. ,callers)
		    (callees 	. ,callees)
		    (beamfile 	. ,beamfile)))
	  fprof-entries)
    (fprof-insert text tag)))

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
	(([tuple rex [tuple ok String]]
	  (display-message-or-buffer string))
	 ([tuple rex [tuple error Reason]]
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

(defun erl-find-source-under-point (node)
  (interactive (list (erl-read-nodename)))
  (let ((mfa (erl-get-call-mfa)))
    (when (null mfa)
      (error "Couldn't determine MFA of call"))
    (apply #'erl-find-source (cons node mfa))))

(defun erl-get-call-mfa ()
  "Return (MODULE FUNCTION ARITY) of the function call under point.
If there is no function call under point, returns nil."
  (save-excursion
    (erl-goto-end-of-call-name)
    (let ((arity (erl-get-arity)))
      (unless (null arity)
	(let ((mf (erlang-get-function-under-point)))
	  (unless (null mf)
	    (let ((module (intern (or (car mf)
				      (erlang-get-module))))
		  (function (intern (cadr mf))))
	      (list module function arity))))))))

(defun erl-goto-end-of-call-name ()
  "Go to the end of the function or module:function part of a function call."
  ;; We basically just want to do forward-sexp iff we're not already
  ;; in the right place
  (unless (member (char-syntax (char-after (point)))
		  '(?w ?_))
    (backward-sexp))
  (forward-sexp))

(defun erl-get-arity ()
  "Get the number of arguments in a function call.
Should be called with point directly before the opening `('."
  ;; Adapted from erlang-get-function-arity.
  (and (looking-at "[\n\r ]*(")
       (save-excursion
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
	   (error nil)))))

(defun erl-find-source (node module &optional function arity)
  "Find the source code for MODULE in a buffer, loading it if necessary.
When FUNCTION is specified, the point is moved to its start."
  (erl-spawn
    (erl-send-rpc node 'distel 'find_source (list module))
    (erl-receive (function arity)
	(([tuple rex [tuple ok Path]]
	  (find-file path)
	  (when function
	    (erl-search-function function arity)))
	 ([tuple rex [error Reason]]
	  (error "%S" reason))))))

(defun erl-search-function (function arity)
  (let ((origin (point))
	(str (concat "\n" (symbol-name function) "("))
	(searching t))
    (goto-char (point-min))
    (while searching
      (cond ((search-forward str nil t)
	     (beginning-of-line)
	     (when (eq (erlang-get-function-arity) arity)
	       (setq searching nil)))
	    (t
	     (setq searching nil)
	     (goto-char origin)
	     (message "Couldn't find function %S/%S" function arity))))))

(defun erl-read-symbol-or-nil (prompt)
  (let ((s (read-string prompt)))
    (if (string= s "")
	nil
      (intern s))))

(provide 'erl-service)

