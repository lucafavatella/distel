;;; edb.el --- Erlang debugger front-end

(require 'erl)
(require 'erl-service)
(require 'erlang)
(require 'ewoc)

;; ----------------------------------------------------------------------
;; EDB minor mode for erlang-mode source files

(define-minor-mode erlang-edb-mode
  "Minor mode extending erlang-mode with debugging commands."
  nil
  " EDB"
  '(("\C-c\C-di" . edb-toggle-interpret)
    ("\C-c\C-db" . edb-toggle-breakpoint)
    ("\C-c\C-dm" . edb-monitor)))

(defun edb-toggle-interpret (node module)
  (interactive (list (erl-read-nodename)
		     (edb-module)))
  (erl-spawn
    (erl-send-rpc node 'distel 'debug_toggle (list module))
    (erl-receive (module)
	(([tuple rex interpreted]
	  (message "Interpreting: %S" module))
	 ([tuple rex uninterpreted]
	  (message "Stopped interpreting: %S" module))
	 ([tuple rex error]
	  (message "Failed!"))))))

(defun edb-module ()
  (if (erlang-get-module)
      (intern (erlang-get-module))
    (error "Can't determine module for current buffer")))

(defun edb-toggle-breakpoint (node module line)
  (interactive (list (erl-read-nodename)
		     (edb-module)
		     (edb-line-number)))
  (erl-spawn
    (erl-send-rpc node 'distel 'break_toggle (list module line))
    (erl-receive (module line)
	(([tuple rex enabled]
	  (message "Enabled breakpoint at %S:%S" module line))
	 ([tuple rex disabled]
	  (message "Disabled breakpoint at %S:%S" module line))))))


(defun edb-line-number ()
  "Current line number."
  ;; Taken from `count-lines' in gud.el
  (save-restriction
    (widen)
    (+ (count-lines 1 (point))
       (if (bolp) 1 0))))

;; ----------------------------------------------------------------------
;; Monitor process

(defvar edb-monitor-mode-map nil
  "Keymap for Erlang debug monitor mode.")

(unless edb-monitor-mode-map
  (setq edb-monitor-mode-map (make-sparse-keymap))
  (define-key edb-monitor-mode-map [?a] 'edb-attach-command)
  (define-key edb-monitor-mode-map [?q] 'erl-quit-viewer))

(defvar edb-processes nil
  "EWOC of processes running interpreted code.")

(defstruct (edb-process
	    (:constructor nil)
	    (:constructor make-edb-process (pid mfa status info)))
  pid mfa status info)

(defun edb-monitor-mode ()
  (interactive)
  (kill-all-local-variables)
  (setq buffer-read-only t)
  (setq erl-old-window-configuration (current-window-configuration))
  (use-local-map edb-monitor-mode-map)
  (setq mode-name "EDB Monitor")
  (setq major-mode 'edb-monitor))

(defun edb-monitor-insert-process (p)
  (let ((buffer-read-only nil)
	(text (edb-monitor-format (erl-pid-to-string (edb-process-pid p))
				  (edb-process-mfa p)
				  (edb-process-status p)
				  (edb-process-info p))))
    (put-text-property 0 (length text) 'erl-pid (edb-process-pid p) text)
    (insert text)))

(defun edb-monitor-format (pid mfa status info)
  (format "%s %s %s %s"
	  (padcut pid 12)
	  (padcut mfa 21)
	  (padcut status 9)
	  (padcut info 21)))

(defun padcut (s w)
  (let ((len (length s)))
    (cond ((= len w) s)
	  ((< len w) (concat s (make-string (- w len) ? )))
	  ((> len w) (substring s 0 w)))))

(defun edb-monitor-header ()
  (edb-monitor-format "PID" "Initial Call" "Status" "Info"))

(defun edb-monitor (node)
  (interactive (list (erl-read-nodename)))
  (let ((bufname (edb-monitor-buffer-name node)))
    (if (get-buffer bufname)
	(display-buffer bufname)
      (edb-start-monitor node))))

(defun edb-monitor-buffer-name (node)
  (format "*edb %S*" node))

(defun edb-start-monitor (node)
  (erl-spawn
    (rename-buffer (edb-monitor-buffer-name node))
    (edb-monitor-mode)
    (erl-send-rpc node 'distel 'debug_subscribe (list erl-self))
    (erl-receive (node)
	(([tuple rex Snapshot]
	  (setq edb-processes
		(ewoc-create 'edb-monitor-insert-process
			     (edb-monitor-header)))
	  (mapc (lambda (item)
		  (pmatch [tuple Pid MFA Status Info] item
		    (ewoc-enter-last edb-processes
				     (make-edb-process pid
						       mfa
						       status
						       info))))
		snapshot)
	  (display-buffer (current-buffer))
	  (&edb-monitor-loop))))))

(defun &edb-monitor-loop ()
  (erl-receive ()
      (( (int (new_status Pid Status Info))
	 (let ((proc (edb-monitor-lookup pid)))
	   (if (null proc)
	       (message "Unknown process: %s" (erl-pid-to-string pid))
	     (setf (edb-process-status proc) status)
	     (setf (edb-process-info proc) info))))
       ( (int (new_process (Pid MFA Status Info)))
	 (ewoc-enter-last edb-processes
			  (make-edb-process pid
					    mfa
					    status
					    info))))
    (ewoc-refresh edb-processes)    
    (&edb-monitor-loop)))

(defun edb-monitor-lookup (pid)
  (car (ewoc-collect edb-processes
		     (lambda (p) (equal (edb-process-pid p) pid)))))

;; ----------------------------------------------------------------------
;; Attach process

(make-variable-buffer-local
 (defvar edb-pid nil
   "Pid of attached process."))

(make-variable-buffer-local
 (defvar edb-node nil
   "Node of attached process."))

(make-variable-buffer-local
 (defvar edb-module nil
   "Current module source code in attach buffer."))

(make-variable-buffer-local 
 (defvar edb-variables-buffer nil
   "Buffer showing variable bindings of attached process."))

;; Attach setup

(defun edb-attach-command ()
  (interactive)
  (let ((pid (get-text-property (point) 'erl-pid)))
    (if pid
	(edb-attach pid)
      (error "No process at point."))))

(defun edb-attach (pid)
  (let ((old-window-config (current-window-configuration)))
    (delete-other-windows)
    (switch-to-buffer (edb-attach-buffer pid))
    (setq erl-old-window-configuration old-window-config)))

(defun edb-attach-buffer (pid)
  (let ((bufname (edb-attach-buffer-name pid)))
    (or (get-buffer bufname)
	(edb-new-attach-buffer pid))))

(defun edb-new-attach-buffer (pid)
  "Start a new attach process and returns its buffer."
  (erl-pid->buffer
   (erl-spawn
     (rename-buffer (edb-attach-buffer-name pid))
     (erlang-mode)
     (edb-attach-mode t)
     (save-excursion (edb-make-variables-window))
     (setq buffer-read-only t)
     (erl-send-rpc (erl-pid-node pid)
		   'distel 'debug_attach (list erl-self pid))
     (erl-receive ()
	 (([tuple rex Pid]
	   (assert (erl-pid-p pid))
	   (setq edb-pid pid)
	   (setq edb-node (erl-pid-node pid))))
       (&edb-attach-init)))))

;; Variables listing window

(defun edb-make-variables-window ()
  "Make a window and buffer for viewing variable bindings.
The *Variables* buffer is killed with the current buffer."
  (split-window-vertically (edb-variables-window-height))
  (let ((vars-buf (generate-new-buffer "*Variables*")))
    (setq edb-variables-buffer vars-buf)
    (make-local-variable 'kill-buffer-hook)
    (add-hook 'kill-buffer-hook
	      (lambda () (kill-buffer edb-variables-buffer)))
    (other-window 1)
    (switch-to-buffer vars-buf)
    (other-window -1)))

(defun edb-variables-window-height ()
  (- (min (/ (window-height) 2) 12)))

;; Attach process states

(defun &edb-attach-init ()
  "Handle initial {attached, Module, Line, Trace}, then enter attach loop."
  (erl-receive ()
      (([tuple meta [tuple attached Mod Line Trace]]
	(if (eq mod 'null)
	    (&edb-attach-loop)
	  (&edb-attach-find-source mod line))))))

(defun &edb-attach-loop ()
  "Attached process loop."
  (erl-receive ()
      (([tuple meta [tuple break_at Mod Line Trace]]
	(let ((msg (format "Status: break at %S:%S" mod line)))
	  (setq header-line-format msg))
	(&edb-attach-goto-source mod line))
       ([tuple meta Status]
	(unless (memq status '(running idle))
	  (message "Unrecognised status: %S" status))
	(setq header-line-format (format "Status: %S" status))
	(setq overlay-arrow-position nil)
	(&edb-attach-loop))
       ([tuple variables Vars]
	;; {variables, [{Name, String}]}
	(when (buffer-live-p edb-variables-buffer)
	  (with-current-buffer edb-variables-buffer
	    (erase-buffer)
	    (mapc (lambda (b)
		    (insert (format "%s\n" b)))
		  vars)))
	(&edb-attach-loop))
       ([tuple message Msg]
	(message msg)
	(&edb-attach-loop))
       (Other
	(message "Other: %S" other)
	(&edb-attach-loop)))))

(defun &edb-attach-goto-source (module line)
  "Display MODULE:LINE in the attach buffer and reenter attach loop."
  (if (eq edb-module module)
      (progn (edb-attach-goto-line line)
	     (&edb-attach-loop))
    (&edb-attach-find-source module line)))

(defun &edb-attach-find-source (module line)
  "Load the source code for MODULE into current buffer at LINE.
Once loaded, reenters the attach loop."
  (erl-send-rpc edb-node 'distel 'find_source (list module))
  (erl-receive (module line)
      (([tuple rex [tuple ok Path]]
	(if (file-regular-p path)
	    (progn (setq edb-module module)
		   (let ((buffer-read-only nil))
		     (erase-buffer)
		     (insert-file-contents path))
		   (edb-attach-goto-line line))
	  (message "No such file: %s" path))))
    (&edb-attach-loop)))

(defun edb-attach-goto-line (line)
  (goto-line line)
  (setq overlay-arrow-string "=>")
  (setq overlay-arrow-position (copy-marker (point))))

(defun edb-attach-buffer-name (pid)
  (format "*edbproc %s on %S*"
	  (erl-pid-to-string pid)
	  (erl-pid-node pid)))

;; ----------------------------------------------------------------------
;; Attach minor mode and commands

(define-minor-mode edb-attach-mode
  "Minor mode for view of attached process source code."
  nil
  " (attached)"
  '(([? ] . edb-attach-step)
    ([?n] . edb-attach-next)
    ([?c] . edb-attach-continue)
    ([?q] . erl-quit-viewer)))

(defun edb-attach-step ()
  (interactive)
  (edb-attach-meta-cmd 'step))
(defun edb-attach-next ()
  (interactive)
  (edb-attach-meta-cmd 'next))
(defun edb-attach-continue ()
  (interactive)
  (edb-attach-meta-cmd 'continue))

(defun edb-attach-meta-cmd (cmd)
  (erl-send edb-pid `[tuple emacs meta ,cmd]))

(provide 'edb)
