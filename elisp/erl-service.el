;;; erl-service.el --- High-level calls to Erlang services.

;;; Commentary:
;;
;; This module implements Emacs commands - i.e. M-x'able key-bind'able
;; sort - for doing stuff with Erlang nodes.
;;
;; The general implementation strategy is to make RPCs to the "distel"
;; erlang module, which does most of the work for us.

(require 'erlang)

(add-hook 'erl-nodeup-hook 'erl-check-backend)

(defun erl-check-backend (node _fsm)
  "Check if we have the 'distel' module available on `node', and popup
a warning if not."
  (unless distel-inhibit-backend-check
    (erl-spawn
      (erl-send `[rex ,node]
		`[,erl-self [call
			     code ensure_loaded (distel)
			     ,(erl-group-leader)]])
      (erl-receive (node)
	  ((['rex ['error _]]
	    (with-current-buffer (get-buffer-create "*Distel Warning*")
	      (erase-buffer)
	      (insert (format "\
Distel Warning: node `%s' can't seem to load the `distel' module.

This means that most Distel commands won't function correctly, because
the supporting library is not available. Please check your code path,
and make sure that Distel's \"ebin\" directory is included.

The most likely cause of this problem is either:

  a) Your ~/.erlang file doesn't add Distel to your load path (the
     Distel \"make config_install\" target can do this for you.)

  b) Your system's boot script doesn't consult your ~/.erlang file to
     read your code path setting.

To disable this warning in future, set `distel-inhibit-backend-check' to t.

"
			      node))
	      (display-buffer (current-buffer))))
	   (other t))))))

(defvar erl-nodename-cache nil
  "The name of the node most recently contacted, for reuse in future
commands. Using C-u to bypasses the cache.")

(defun erl-read-nodename ()
  "Read a node name, either from a cached variable or the minibuffer.
If a universal (C-u) prefix argument is in effect, the cache in skipped."
  (if (and (not (consp current-prefix-arg))
	   erl-nodename-cache)
      erl-nodename-cache
    (erl-read-nodename-from-user)))

(defun erl-read-nodename-from-user ()
  (let ((name-string (read-string "Node: ")))
    (let ((name (intern (if (string-match "@" name-string)
			    name-string
			  (concat name-string
				  "@" (erl-determine-hostname))))))
      (when (derl-node-p name)
	(setq erl-nodename-cache name))
      name)))

(defun erl-get-nodename ()
  (interactive)
    (erl-read-nodename-from-user))

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
  "Send an RPC request on NODE to apply(MOD, FUN, ARGS).
The reply will be sent back as an asynchronous message of the form:
    [rex Result]
On an error, Result will be [badrpc Reason]."
  (let ((m 'distel)
	(f 'rpc_entry)
	(a (list mod fun args)))
    (erl-send (tuple 'rex node)
	      ;; {Who, {call, M, F, A, GroupLeader}}
	      (tuple erl-self (tuple 'call m f a (erl-group-leader))))))

(defun erl-rpc-receive (k kargs)
  "Receive the reply to an `erl-rpc'."
  (erl-receive (k kargs)
      ((['rex reply] (apply k (cons reply kargs))))))

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
    (setq buffer-read-only t)
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
  (define-key process-list-mode-map [?u] 'erl-process-list)
  (define-key process-list-mode-map [?q] 'erl-quit-viewer)
  (define-key process-list-mode-map [?k] 'erl-pman-kill-process)
  (define-key process-list-mode-map [return] 'erl-show-process-info)
  (define-key process-list-mode-map [(control m)] 'erl-show-process-info)
  (define-key process-list-mode-map [?i] 'erl-show-process-info-item)
  (define-key process-list-mode-map [?b] 'erl-show-process-backtrace)
  (define-key process-list-mode-map [?m] 'erl-show-process-messages))

(defun process-list-mode ()
  "Major mode for viewing Erlang process listings.

Available commands:

\\[erl-quit-viewer]	- Quit the process listing viewer, restoring old window config.
\\[erl-process-list]	- Update the process list.
\\[erl-pman-kill-process]	- Send an EXIT signal with reason 'kill' to process at point.
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
    (cond ((null pid)
	   (message "No process at point."))
	  ((string= "" item)
	   (erl-show-process-info))
	  (t
	   (erl-spawn
	     (erl-send-rpc (erl-pid-node pid)
			   'distel 'process_info_item (list pid item))
	     (erl-receive (item pid)
		 ((['rex ['ok string]]
		   (display-message-or-view string "*pinfo item*"))
		  (other
		   (message "Error from erlang side of process_info:\n  %S"
			    other)))))))))

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
	(goto-char (point-min))
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

(defun erl-pman-kill-process ()
  "Kill process at point in a summary buffer."
  (interactive)
  (let ((pid (get-text-property (point) 'erl-pid)))
    (if (null pid)
	(message "No process at point.")
      (message "Sent EXIT (kill) signal ")
      (erl-exit 'kill pid))))

;; ------------------------------------------------------------
;; Single process viewer

(defun erl-view-process (pid)
  (let ((buf (get-buffer (erl-process-view-buffer-name pid))))
    (if buf
	(select-window (display-buffer buf))
      (erl-spawn
	(process-view-mode)
	(setq erl-old-window-configuration (current-window-configuration))
	(setq viewed-pid pid)
	(erl-send-rpc (erl-pid-node pid)
		      'distel 'process_summary_and_trace (list erl-self pid))
	(erl-receive (pid)
	    ((['rex ['error reason]]
	      (message "%s" reason))
	     (['rex ['badrpc reason]]
	      (message "Bad RPC: %s" reason))
	     (['rex summary]
	      (rename-buffer (erl-process-view-buffer-name pid))
	      (erase-buffer)
	      (insert summary)
	      (setq buffer-read-only t)
	      (goto-char (point-min))
	      (select-window (display-buffer (current-buffer)))
	      (&erl-process-trace-loop))
	     (other
	      (message "Unexpected reply: %S" other))))))))

(defun erl-process-view-buffer-name (pid)
  (format "*pinfo %S on %S*"
	  (erl-pid-id pid) (erl-pid-node pid)))

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

(defun &erl-process-trace-loop ()
  (erl-receive ()
      ((['trace_msg text]
	(goto-char (point-max))
	(let ((buffer-read-only nil))
	  (insert text))))
    (&erl-process-trace-loop)))

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
    (fprof-receive-analysis)))

(defun fprof-analyse (node filename)
  "View an existing profiler analysis from a file."
  (interactive (list (erl-read-nodename)
		     (read-string "Filename: ")))
  (erl-spawn
    (erl-send-rpc node 'distel 'fprof_analyse (list filename))
    (fprof-receive-analysis)))

(defun fprof-receive-analysis ()
  (message "Waiting for fprof reply...")
  (erl-receive ()
      ((['rex ['ok preamble header entries]]
	(message "Got fprof reply, drawing...")
	(fprof-display preamble header entries))
       (other (message "Unexpected reply: %S" other)))))


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
    (['process title info-list]
     (insert "\n")
     (insert title "\n")
     (dolist (info info-list)
       (insert "  " info "\n"))
     (insert "\n"))
    (['tracepoint tag mfa text callers callees beamfile]
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
	((['rex ['ok string]]
	  (display-message-or-view string "*Expression Result*"))
	 (['rex ['error reason]]
	  (message "Error: %S" reason))
	 (other
	  (message "Unexpected: %S" other))))))

(defun erl-add-terminator (s)
  "Make sure S terminates with a dot (.)"
  (if (string-match "\\.\\s *$" s)
      s
    (concat s ".")))

(defun erl-reload-module (node module)
  "Reload a module."
  (interactive (list (erl-read-nodename)
		     (let* ((module (erlang-get-module))
			    (prompt (if module
					(format "Module (default %s): " module)
					"Module: ")))
		       (intern (read-string prompt nil nil module)))))
  (if (and (eq node edb-monitor-node)
	   (assq module edb-interpreted-modules))
      (erl-reinterpret-module node module)
    (erl-eval-expression node (format "c:l('%s')." module))))

(defun erl-reinterpret-module (node module)
  ;; int:i(SourcePath).
  (erl-send-rpc node
		'int 'i (list (cadr (assq module edb-interpreted-modules)))))

;; ------------------------------------------------------------
;; Find the source for a module

(defvar erl-find-history-ring (make-ring 20)
  "History ring tracing for following functions to their definitions.")

(defun erl-find-source-under-point ()
  "Goto the source code that defines the function being called at point.
For remote calls, contacts an Erlang node to determine which file to
look in, with the following algorithm:

  Find the directory of the module's beam file (loading it if necessary).
  Look for the source file in:
    Same directory as the beam file
    Again with /ebin/ replaced with /src/
    Again with /ebin/ replaced with /erl/
    Directory where source file was originally compiled

  Otherwise, report that the file can't be found.

When `distel-tags-compliant' is non-nil, or a numeric prefix argument
is given, the user is prompted for the function to lookup (with a
default.)"
  (interactive)
  (let* ((default-mfa (erl-get-call-mfa (erlang-get-module)))
	 (mfa (if (or distel-tags-compliant
		      (integerp current-prefix-arg)
		      (null default-mfa))
		  (erl-prompt-for-tag default-mfa)
		default-mfa)))
    (apply #'erl-find-source mfa)))

(defun erl-prompt-for-tag (mfa)
  (let* ((default
	   (if (null mfa)
	       nil
	     ;; [module:]function[/arity]
	     (concat (if (first mfa)  (format "%s:" (first mfa)) "")
		     (if (second mfa) (format "%s"  (second mfa)) "")
		     (if (third mfa)  (format "/%S" (third mfa))))))
	 (prompt (format "Find erlang function: %s"
			 (if default
			     (format "(default %s) " default)
			   ""))))
    (erl-get-call-mfa-from-string (erlang-get-module)
				  (read-string prompt nil nil default))))

(defun erl-get-call-mfa-from-string (module str)
  (with-temp-buffer
    (with-syntax-table erlang-mode-syntax-table
      (insert str)
      (insert " ")
      (goto-char (point-min))
      (let ((mfa (erl-get-call-mfa module)))
	(when (null mfa)
	  (error "Couldn't determine MFA of call"))
	mfa))))

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

(defun erl-get-call-mfa (&optional module)
  "Return (MODULE FUNCTION ARITY) of the function call under point.
If there is no function call under point, returns nil.
ARITY is returned NIL if it cannot be determined."
  (save-excursion
    (erl-goto-end-of-call-name)
    (let ((arity (erl-get-arity))
	  (mf (erlang-get-function-under-point)))
      (if (null mf)
	  nil
	;; (MODULE FUNCTION ARITY)
	(list (if (or (car mf) module)
		  (intern (or (car mf) module))
		nil)
	      (intern (cadr mf))
	      arity)))))

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

(defun erl-find-source (module &optional function arity)
  "Find the source code for MODULE in a buffer, loading it if necessary.
When FUNCTION is specified, the point is moved to its start."
  ;; Add us to the history list
  (ring-insert-at-beginning erl-find-history-ring
			    (copy-marker (point-marker)))
  (if (equal (symbol-name module) (erlang-get-module))
      (when function
	(erl-search-function function arity))
    (let ((node (erl-read-nodename)))
      (erl-spawn
	(erl-send-rpc node 'distel 'find_source (list module))
	(erl-receive (function arity)
	    ((['rex ['ok path]]
	      (find-file path)
	      (when function
		(erl-search-function function arity)))
	     (['rex ['error reason]]
	      ;; Remove the history marker, since we didn't go anywhere
	      (ring-remove erl-find-history-ring)
	      (message "Error: %s" reason))))))))

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

;; ------------------------------------------------------------
;; Completion of modules and functions

(defun erl-complete (node)
  "Complete the module or remote function name at point."
  (interactive (list (erl-read-nodename)))
  (let* ((end (point))
	 (beg (save-excursion (backward-sexp 1)
			      ;; FIXME: see erl-goto-end-of-call-name
			      (when (eql (char-before) ?:)
				(backward-sexp 1))
			      (point)))
	 (str (buffer-substring-no-properties beg end))
	 (buf (current-buffer))
	 (continuing (equal last-command (cons 'erl-complete str))))
    (setq this-command (cons 'erl-complete str))
    (if (string-match "^\\(.*\\):\\(.*\\)$" str)
	;; completing function in module:function
	(let ((mod (intern (match-string 1 str)))
	      (pref (match-string 2 str))
	      (beg (+ beg (match-beginning 2))))
	  (erl-spawn
	    (erl-send-rpc node 'distel 'functions (list mod pref))
	    (&erl-receive-completions "function" beg end pref buf continuing)))
      ;; completing just a module
      (erl-spawn
	(erl-send-rpc node 'distel 'modules (list str))
	(&erl-receive-completions "module" beg end str buf continuing)))))

(defun &erl-receive-completions (what beg end prefix buf continuing)
  (let ((state (erl-async-state buf)))
    (erl-receive (what state beg end prefix buf continuing)
	((['rex ['ok completions]]
	  (when (equal state (erl-async-state buf))
	    (with-current-buffer buf
	      (erl-complete-thing what continuing beg end prefix completions))))
	 (['rex ['error reason]]
	  (message "Error: %s" reason))
	 (other
	  (message "Unexpected reply: %S" other))))))

(defun erl-async-state (buffer)
  "Return an opaque state for BUFFER.
This is for making asynchronous operations: if the state when we get a
reply is not equal to the state when we started, then the user has
done something - modified the buffer, or moved the point - so we may
want to cancel the operation."
  (with-current-buffer buffer
    (cons (buffer-modified-tick)
	  (point))))

(defun erl-complete-thing (what scrollable beg end pattern completions)
  "Complete a string in the buffer.
WHAT is a string that says what we're completing.
SCROLLABLE is a flag saying whether this is a repeated command that
may scroll the completion list.
BEG and END are the buffer positions around what we're completing.
PATTERN is the string to complete from.
COMPLETIONS is a list of potential completions (strings.)"
  ;; This function, and `erl-maybe-scroll-completions', are basically
  ;; cut and paste programming from `lisp-complete-symbol'. The fancy
  ;; Emacs completion packages (hippie and pcomplete) looked too
  ;; scary.
  (or (and scrollable (erl-maybe-scroll-completions))
      (let* ((completions (erl-make-completion-alist completions))
	     (completion (try-completion pattern completions)))
	(cond ((eq completion t)
	       (message "Sole completion"))
	      ((null completion)
	       (message "Can't find completion for %s \"%s\"" what pattern)
	       (ding))
	      ((not (string= pattern completion))
	       (delete-region beg end)
	       (insert completion))
	      (t
	       (message "Making completion list...")
	       (let ((list (all-completions pattern completions)))
		 (setq list (sort list 'string<))
		 (with-output-to-temp-buffer "*Completions*"
		   (display-completion-list list)))
	       (message "Making completion list...%s" "done"))))))

(defun erl-make-completion-alist (list)
  "Make an alist out of list.
The same elements go in the CAR, and nil in the CDR. To support the
apparently very stupid `try-completions' interface, that wants an
alist but ignores CDRs."
  (mapcar (lambda (x) (cons x nil)) list))

(defun erl-maybe-scroll-completions ()
  "Scroll the completions buffer if it is visible.
Returns non-nil iff the window was scrolled."
  (let ((window (get-buffer-window "*Completions*")))
    (when (and window (window-live-p window) (window-buffer window)
	       (buffer-name (window-buffer window)))
      ;; If this command was repeated, and
      ;; there's a fresh completion window with a live buffer,
      ;; and this command is repeated, scroll that window.
      (with-current-buffer (window-buffer window)
	(if (pos-visible-in-window-p (point-max) window)
	    (set-window-start window (point-min))
	  (save-selected-window
	    (select-window window)
	    (scroll-up))))
      t)))

;; ------------------------------------------------------------
;; Refactoring

(defun erl-refactor-subfunction (node name start end)
  "Refactor the expression(s) in the region as a function.

The expressions are replaced with a call to the new function, and the
function itself is placed on the kill ring for manual placement. The
new function's argument list includes all variables that become free
during refactoring - that is, the local variables needed from the
original function.

New bindings created by the refactored expressions are *not* exported
back to the original function. Thus this is not a \"pure\"
refactoring.

This command requires Erlang syntax_tools package to be available in
the node, version 1.2 (or perhaps later.)"
  (interactive (list (erl-read-nodename)
		     (read-string "Function name: ")
		     (region-beginning)
		     (region-end)))
  (let ((buffer (current-buffer))
	(text   (buffer-substring start end)))
    (erl-spawn
      (erl-send-rpc node 'distel 'free_vars (list text))
      (erl-receive (name start end buffer text)
	  ((['rex ['badrpc rsn]]
	    (message "Refactor failed: %S" rsn))
	   (['rex ['error rsn]]
	    (message "Refactor failed: %s" rsn))
	   (['rex ['ok free-vars]]
	    (with-current-buffer buffer
	      (let ((arglist
		     (concat "(" (mapconcat 'symbol-name free-vars ", ") ")"))
		    (body
		     (buffer-substring start end)))
		;; rewrite the original as a call
		(delete-region start end)
		(insert (format "%s%s" name arglist))
		(indent-according-to-mode)
		;; Now generate the function and stick it on the kill ring
		(kill-new (with-temp-buffer
			    (insert (format "%s%s ->\n%s.\n" name arglist body))
			    (erlang-mode)
			    (indent-region (point-min) (point-max) nil)
			    (buffer-string)))
		(message "Saved `%s' definition on kill ring." name)))))))))

(provide 'erl-service)

