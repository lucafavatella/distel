;;; erl-service.el --- High-level calls to Erlang services.

;;; Commentary:
;;
;; This module implements Emacs commands - i.e. M-x'able key-bind'able
;; sort - for doing stuff with Erlang nodes.
;;
;; The general implementation strategy is to make RPCs to the "distel"
;; erlang module, which does most of the work for us.

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

(defun erl-send-rpc (node m f a)
  (erl-send (tuple 'rex node)
	    ;; {Who, {call, M, F, A, GroupLeader}}
	    (tuple erl-self (tuple 'call m f a erl-group-leader))))
  
(defun erl-rpc-receive (k kargs)
  "Receive the reply to an `erl-rpc'."
  (erl-receive (k kargs)
    ([tuple rex Reply] (apply k (cons reply kargs)))))

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

(unless process-list-mode-map
  (setq process-list-mode-map (make-sparse-keymap))
  (define-key process-list-mode-map [?q] 'erl-quit-viewer)
  (define-key process-list-mode-map [return] 'erl-show-process-info)
  (define-key process-list-mode-map [(control m)] 'erl-show-process-info))

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
    ([tuple trace_msg Text]
     (goto-char (point-max))
     (insert text)
     (erl-process-trace-loop))))

(provide 'erl-service)

