;;; edb.el --- Erlang debugger front-end

(require 'cl)
(require 'erl)
(require 'erl-service)
(require 'erlang)
(require 'ewoc)

;; ----------------------------------------------------------------------
;; Configurables

(defvar edb-popup-monitor-on-event t
  "*Automatically popup the monitor on interesting events.
An interesting event is an unattached process reaching a breakpoint,
or an attached process exiting.")

;; ----------------------------------------------------------------------
;; Integration with erlang-extended-mode buffers.

(make-variable-buffer-local
 (defvar edb-module-interpreted nil
   "Non-nil means that the buffer's Erlang module is interpreted.
This variable is meaningful in erlang-extended-mode buffers.
The interpreted status refers to the node currently being monitored by
edb."))

(defun edb-update-interpreted-status ()
  "Update `edb-module-interpreted' for current buffer."
  (when erlang-extended-mode
    (setq edb-module-interpreted
	  (if (member (edb-source-file-module-name) edb-interpreted-modules)
	      t
	    nil))
    (force-mode-line-update)))

(add-hook 'erlang-extended-mode-hook 'edb-update-interpreted-status)

;; ----------------------------------------------------------------------
;; EDB minor mode for erlang-mode source files

(defun edb-toggle-interpret (node module)
  "Toggle debug-interpreting of the current buffer's module."
  (interactive (list (erl-read-nodename)
		     (edb-module)))
  (when (edb-ensure-monitoring node)
    (erl-spawn
      (erl-set-name "EDB RPC to toggle interpretation of %S on %S"
		    module node)
      (erl-send-rpc node 'distel 'debug_toggle (list module))
      (erl-receive (module)
	  (([rex interpreted]
	    (message "Interpreting: %S" module))
	   ([rex uninterpreted]
	    (message "Stopped interpreting: %S" module))
	   ([rex error]
	    (message "Failed!")))))))

(defun edb-module ()
  (if (erlang-get-module)
      (intern (erlang-get-module))
    (error "Can't determine module for current buffer")))

(defun edb-toggle-breakpoint (node module line)
  "Toggle a breakpoint on the current line."
  (interactive (list (erl-read-nodename)
		     (edb-module)
		     (edb-line-number)))
  (when (edb-ensure-monitoring node)
    (erl-spawn
      (erl-set-name "EDB RPC to toggle of breakpoint %S:%S on %S"
		    module line node)
      (erl-send-rpc node 'distel 'break_toggle (list module line))
      (erl-receive (module line)
	  (([rex enabled]
	    (message "Enabled breakpoint at %S:%S" module line))
	   ([rex disabled]
	    (message "Disabled breakpoint at %S:%S" module line)))))))

(defun edb-line-number ()
  "Current line number."
  ;; Taken from `count-lines' in gud.el
  (save-restriction
    (widen)
    (+ (count-lines 1 (point))
       (if (bolp) 1 0))))

;; ----------------------------------------------------------------------
;; Monitor process

(defvar edb-monitor-buffer nil
  "Monitor process/viewer buffer.")

(defvar edb-monitor-node nil
  "Node we are debug-monitoring.")

(defvar edb-monitor-mode-map nil
  "Keymap for Erlang debug monitor mode.")

(defvar edb-interpreted-modules '()
  "Set of modules being interpreted on the currently monitored node.")

(defvar edb-breakpoints '()
  "Set of (MODULE . LINE) breakpoints on the currently monitored node.")

(unless edb-monitor-mode-map
  (setq edb-monitor-mode-map (make-sparse-keymap))
  (define-key edb-monitor-mode-map [return] 'edb-attach-command)
  (define-key edb-monitor-mode-map [(control m)] 'edb-attach-command)
  (define-key edb-monitor-mode-map [?a] 'edb-attach-command)
  (define-key edb-monitor-mode-map [?q] 'erl-bury-viewer)
  (define-key edb-monitor-mode-map [?k] 'erl-quit-viewer))

(defvar edb-processes nil
  "EWOC of processes running interpreted code.")

(defstruct (edb-process
	    (:constructor nil)
	    (:constructor make-edb-process (pid mfa status info)))
  pid mfa status info)

(defun edb-monitor-mode ()
  "Major mode for viewing debug'able processes.

Available commands:
\\[edb-attach-command]	- Attach to the process at point.
\\[erl-bury-viewer]	- Hide the monitor window.
\\[erl-quit-viewer]	- Quit monitor."
  (interactive)
  (kill-all-local-variables)
  (setq buffer-read-only t)
  (setq erl-old-window-configuration (current-window-configuration))
  (use-local-map edb-monitor-mode-map)
  (setq mode-name "EDB Monitor")
  (setq major-mode 'edb-monitor-mode))

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
  (when (edb-ensure-monitoring node)
    (unless (get-buffer-window edb-monitor-buffer)
      ;; Update the restorable window configuration
      (with-current-buffer edb-monitor-buffer
	(setq erl-old-window-configuration
	      (current-window-configuration))))
    (pop-to-buffer edb-monitor-buffer)))

(defun edb-ensure-monitoring (node)
  "Make sure the debug monitor is watching the node.
Returns NIL if this cannot be ensured."
  (if (edb-monitor-node-change-p node)
      (when (y-or-n-p (format "Attach debugger to %S instead of %S? "
			      node edb-monitor-node))
	;; Kill existing edb then start again
	(kill-buffer edb-monitor-buffer)
	(edb-start-monitor node))
    (if (edb-monitor-live-p)
	t
      (edb-start-monitor node))))

(defun edb-monitor-node-change-p (node)
  "Do we have to detach/reattach to debug on NODE?"
  (and (edb-monitor-live-p)
       (not (equal node edb-monitor-node))))

(defun edb-monitor-live-p ()
  "Are we actively debug-monitoring a node?"
  (and edb-monitor-buffer
       (buffer-live-p edb-monitor-buffer)))

(defun edb-monitor-buffer-name (node)
  (format "*edb %S*" node))

(defun edb-start-monitor (node)
  "Start debug-monitoring NODE."
  (erl-spawn
    (erl-set-name "EDB Monitor on %S" node)
    (setq edb-monitor-node node)
    (setq edb-monitor-buffer (current-buffer))
    (rename-buffer (edb-monitor-buffer-name node))
    (edb-monitor-mode)
    (add-hook 'kill-buffer-hook 'edb-monitor-cleanup)
    (erl-send-rpc node 'distel 'debug_subscribe (list erl-self))
    (erl-receive (node)
	(([rex [Interpreted Breaks Snapshot]]
	  (setq edb-interpreted-modules interpreted)
	  (setq edb-breakpoints breaks)
	  (edb-update-source-buffers)
	  (setq edb-processes
		(ewoc-create 'edb-monitor-insert-process
			     (edb-monitor-header)))
	  (mapc (lambda (item)
		  (pmatch [Pid MFA Status Info] item
		    (ewoc-enter-last edb-processes
				     (make-edb-process pid
						       mfa
						       status
						       info))))
		snapshot)
	  (&edb-monitor-loop))))))

(defun &edb-monitor-loop ()
  "Monitor process main loop.
Tracks events and state changes from the Erlang node."
  (erl-receive ()
      (([int [new_status Pid Status Info]]
	 (let ((proc (edb-monitor-lookup pid)))
	   (if (null proc)
	       (message "Unknown process: %s" (erl-pid-to-string pid))
	     (setf (edb-process-status proc) (symbol-name status))
	     (setf (edb-process-info proc) info)
	     (when (and edb-popup-monitor-on-event
			(edb-interesting-event-p pid status info))
	       (display-buffer (current-buffer))))))
       ;;
       ([int [new_process (Pid MFA Status Info)]]
	 (ewoc-enter-last edb-processes
			  (make-edb-process pid
					    mfa
					    (symbol-name status)
					    info)))
       ;;
       ([int [interpret Mod]]
	(push mod edb-interpreted-modules)
	(edb-update-source-buffers mod))
       ;;
       ([int [no_interpret Mod]]
	(setq edb-interpreted-modules (remq mod edb-interpreted-modules))
	(edb-update-source-buffers mod))
       ;;
       ([int [no_break Mod]]
	(setq edb-breakpoints
	      (remove-if (lambda (bp)
			   (mcase bp
			     ([,mod _] t)
			     (_        nil)))
			 edb-breakpoints)))
       ;;
       ([int [new_break [Pos _Info]]]
	(push pos edb-breakpoints))
       ;;
       ([int [delete_break Pos]]
	(setq edb-breakpoints
	      (remove pos edb-breakpoints))))
    (ewoc-refresh edb-processes)    
    (&edb-monitor-loop)))

(defun edb-interesting-event-p (pid status info)
  (or (and (eq status 'exit)
	   (edb-attached-p pid))
      (and (eq status 'break)
	   (not (edb-attached-p pid)))))

(defun edb-update-source-buffers (&optional mod)
  "Update the debugging state of all Erlang buffers.
When MOD is given, only update those visiting that module."
  (mapc (lambda (buf)
	  (with-current-buffer buf
	    (when (and erlang-extended-mode
		       (or (null mod)
			   (eq (edb-source-file-module-name) mod)))
	      (edb-update-interpreted-status))))
	(buffer-list)))

(defun edb-source-file-module-name ()
  "Return the Erlang module of the current buffer as a symbol, or NIL."
  (let ((name (erlang-get-module)))
    (if name (intern name) nil)))

(defun edb-monitor-lookup (pid)
  (car (ewoc-collect edb-processes
		     (lambda (p) (equal (edb-process-pid p) pid)))))

(defun edb-monitor-cleanup ()
  "Cleanup state after the edb process exits."
  (setq edb-interpreted-modules nil)
  (setq edb-breakpoints nil)
  (edb-update-source-buffers))

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

(defvar edb-attach-with-new-frame nil
  "When true, attaching to a process opens a new frame.")

;; Attach setup

(defun edb-attach-command ()
  (interactive)
  (let ((pid (get-text-property (point) 'erl-pid)))
    (if pid
	(progn (when edb-attach-with-new-frame 
		 (select-frame (make-frame)))
	       (edb-attach pid))
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
     (erl-set-name "EDB Attach to process %S on %S"
		   (erl-pid-id pid)
		   (erl-pid-node pid))
     (rename-buffer (edb-attach-buffer-name pid))
     (erlang-mode)
     (edb-attach-mode t)
     (message "Entered debugger. Press 'h' for help.")
     (setq buffer-read-only t)
     (erl-send-rpc (erl-pid-node pid)
		   'distel 'debug_attach (list erl-self pid))
     (erl-receive ()
	 (([rex Pid]
	   (assert (erl-pid-p pid))
	   (setq edb-pid pid)
	   (setq edb-node (erl-pid-node pid))
	   (save-excursion (edb-make-variables-window))))
       (&edb-attach-loop)))))

;; Variables listing window

(defun edb-make-variables-window ()
  "Make a window and buffer for viewing variable bindings.
The *Variables* buffer is killed with the current buffer."
  (split-window-vertically (edb-variables-window-height))
  (let ((vars-buf (edb-make-variables-buffer)))
    (setq edb-variables-buffer vars-buf)
    (make-local-variable 'kill-buffer-hook)
    (add-hook 'kill-buffer-hook
	      (lambda () (kill-buffer edb-variables-buffer)))
    (other-window 1)
    (switch-to-buffer vars-buf)
    (other-window -1)))

(defun edb-variables-window-height ()
  (- (min (/ (window-height) 2) 12)))

(defun edb-make-variables-buffer ()
  "Create the edb variable list buffer."
  (let ((meta-pid edb-pid))
    (with-current-buffer (generate-new-buffer "*Variables*")
      (edb-variables-mode)
      (setq edb-pid meta-pid)
      (current-buffer))))

(defun edb-variables-mode ()
  (kill-all-local-variables)
  (setq major-mode 'edb-variables)
  (setq mode-name "EDB Variables")
  (setq buffer-read-only t)
  (use-local-map edb-variables-mode-map))

(defvar edb-variables-mode-map nil
  "Keymap for EDB variables viewing.")

(when (null edb-variables-mode-map)
  (setq edb-variables-mode-map (make-sparse-keymap))
  (define-key edb-variables-mode-map [?m]          'edb-show-variable)
  (define-key edb-variables-mode-map [(control m)] 'edb-show-variable))

(defun edb-show-variable ()
  "Pop a window showing the full value of the variable at point."
  (interactive)
  (let ((var (get-text-property (point) 'edb-variable-name)))
    (if (null var)
	(message "No variable at point")
      (edb-attach-meta-cmd `[get_binding ,var]))))

;; Attach process states

(defun &edb-attach-loop ()
  "Attached process loop."
  (erl-receive ()
      (([location Mod Line Pos Max]
 	(let ((msg (format "Location: %S:%S (Stack pos: %S/%S)"
			   mod line pos max)))
 	  (setq header-line-format msg))
	(&edb-attach-goto-source mod line))
       ([status Status]
	(unless (memq status '(running idle))
	  (message "Unrecognised status: %S" status))
	(setq header-line-format (format "Status: %S" status))
	(setq overlay-arrow-position nil)
	(&edb-attach-loop))
       ([variables Vars]
	;; {variables, [{Name, String}]}
	(when (buffer-live-p edb-variables-buffer)
	  (with-current-buffer edb-variables-buffer
	    (let ((buffer-read-only nil))
	      (erase-buffer)
	      (mapc (lambda (b)
		      (let ((name   (tuple-elt b 1))
			    (string (tuple-elt b 2)))
			(put-text-property 0 (length string)
					   'edb-variable-name name
					   string)
			(insert string)))
		    vars))))
	(&edb-attach-loop))
       ([message Msg]
	(message msg)
	(&edb-attach-loop))
       ([show_variable Value]
	(save-excursion (display-message-or-view value "*Variable Value*"))
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
      (([rex [ok Path]]
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

(defun edb-attached-p (pid)
  "Non-nil when we have an attach buffer viewing PID."
  (buffer-live-p (get-buffer (edb-attach-buffer-name pid))))

;; ----------------------------------------------------------------------
;; Attach minor mode and commands

(define-minor-mode edb-attach-mode
  "Minor mode for debugging an Erlang process.

Available commands:
\\<edb-attach-mode-map>
\\[edb-attach-help]	- Popup this help text.
\\[erl-quit-viewer]	- Quit the viewer (doesn't kill the process)
\\[edb-attach-step]	- Step (into expression)
\\[edb-attach-next]	- Next (over expression)
\\[edb-attach-up]	- Up to the next stack frame
\\[edb-attach-down]	- Down to the next stack frame
\\[edb-attach-continue]	- Continue (until breakpoint)"
  nil
  " (attached)"
  '(([? ] . edb-attach-step)
    ([?n] . edb-attach-next)
    ([?c] . edb-attach-continue)
    ([?u] . edb-attach-up)
    ([?d] . edb-attach-down)
    ([?q] . erl-quit-viewer)
    ([?h] . edb-attach-help)))

(defun edb-attach-help ()
  (interactive)
  (describe-function 'edb-attach-mode))

(defun edb-attach-step ()
  (interactive)
  (edb-attach-meta-cmd 'step))
(defun edb-attach-next ()
  (interactive)
  (edb-attach-meta-cmd 'next))
(defun edb-attach-continue ()
  (interactive)
  (edb-attach-meta-cmd 'continue))
(defun edb-attach-up ()
  (interactive)
  (edb-attach-meta-cmd 'up))
(defun edb-attach-down ()
  (interactive)
  (edb-attach-meta-cmd 'down))

(defun edb-attach-meta-cmd (cmd)
  (erl-send edb-pid `[emacs meta ,cmd]))

(provide 'edb)
