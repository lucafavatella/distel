;;; erl.el --- Erlang-style process runtime system.

;;; Commentary:
;;
;; This module provides an Erlang-like runtime system in
;; Emacs. Processes are emacs buffers with local variables containing
;; the pid, mailbox, etc.
;;
;; When a process is spawned it gets assigned a pid and a new buffer,
;; and its initial function is called in that buffer. This function
;; can do some initial processing, and then call (erl-continue K),
;; where K is a continuation function to called the next time the
;; process is scheduled. Usually the process won't be scheduled until
;; it receives a new message, which it can then retreive from its
;; mailbox. If the process returns without setting a new continuation,
;; it terminates with 'normal' status.

(require 'cl)
(require 'derl)
(require 'erl-service)
(require 'patmatch)

;; Process ID structure.
;;
;; Exactly matches the [erl-pid NODE ID SERIAL CREATION] vector used
;; in the `erlext' mapping, so don't change it!
(defstruct (erl-pid
	    (:type vector)
	    :named
	    (:constructor nil)		; no default constructor
	    (:constructor make-erl-pid (node id serial creation))
	    (:constructor make-erl-local-pid (&optional (id (incf erl-pid-counter))
							(node erl-node-name)
							(serial 0)
							(creation 0))))
  node id serial creation)

;; Global book keeping state

(defvar erl-node-name
  (intern (concat "erlmacs@" (getenv "HOSTNAME")))
  "Node name for Emacs.")

(defvar erl-pid-counter 0
  "Counter for PIDs.")

(defvar erl-process-buffer-alist nil
  "Automatically-maintained association list of (PID-ID . BUFFER)
mappings for local processes.")

(defconst erl-null-pid (make-erl-local-pid 0)
  "\"Null process\", the /dev/null of erl processes.
Any messages sent to this process are quietly discarded. When code
isn't running in the buffer of a particular process, it's running as
the null process.")

(defvar erl-schedulable-processes nil
  "List of processes which can be scheduled to run.")

(defvar erl-in-scheduler-loop nil
  "True when the scheduler loop is on the call stack, i.e. when
schedulable processes are guaranteed to be executed before control is
passed back to emacs.")

(defvar erl-default-group-leader erl-null-pid
  ;; Initialized to a real process further down
  "Default group_leader for new processes.
Processes spawned by other processes will inherit their GL, but
\"brand new\" ones will use this.")

(defvar erl-popup-on-output t
  "Popup *erl-output* when new output arrives.")

;; Process-local variables

(defmacro defprocvar (symbol &optional initvalue docstring)
  "Define SYMBOL as a buffer-local process variable."
  `(prog1 (defvar ,symbol ,initvalue ,docstring)
     (make-variable-buffer-local ',symbol)
     ;; stop major modes' `kill-all-local-variables' from rubbing out
     ;; the process state
     (put ',symbol 'permanent-local t)))

;; FIXME - what's the right incantation to have defprocvar fontified
;; as a keyword?

(defprocvar erl-self erl-null-pid
  "Current process' pid.
Always bound in a process' buffer, but in
other buffers defaults to `erl-null-pid'.")
(defprocvar erl-mailbox nil
  "Process mailbox.
Contains messages for the process, which it's supposed to process and
remove. Messages are ordered from oldest to newest.")
(defprocvar erl-group-leader nil
  "Group leader process.")
(defprocvar erl-continuation nil
  "Function for the scheduler to call to run the process.")
(defprocvar erl-continuation-args nil
  "Arguments for continuation function.")
(defprocvar erl-links nil
  "Process links.")
(defprocvar erl-trap-exit nil
  "True when trapping exits from linked processes.")
(defprocvar erl-exit-reason nil
  "Exit reason, or nil if the process is alive.")

(defmacro with-erl-process (pid &rest body)
  "Execute BODY in PID's buffer. This is a full context-switch."
  `(with-current-buffer (erl-pid->buffer ,pid)
     ,@body))

;; Bindings capture helpers

(defmacro capture-bindings (&rest vars)
  "Create a data structure representing the bidings of VARS.
These bindings can be restored by `with-bindings' or
`call-with-bindings'."
  `(list ',vars (list ,@vars)))

(defmacro with-bindings (bindings &rest body)
  "Run BODY with BINDINGS restored.
BINDINGS is created by `capture-bindings'"
  `(call-with-bindings ,bindings (lambda () ,@body)))

(defun call-with-bindings (bindings fn)
  "Call FN with BINDINGS restored.
BINDINGS is created by `capture-bindings'"
  (let ((vars (car bindings))
	(vals (cadr bindings)))
    (eval `(apply (lambda ,vars (funcall ,fn)) ',vals))))

;; Process API functions

(defmacro erl-spawn-async (&rest body)
  "Spawn a new erl process and have it execute BODY.

Since the process may be scheduled for later, BODY is not guaranteed
to run in the current dynamic environment; see `erl-spawn' for an
alternative."
  `(erl-spawn-fun (lambda () ,@body)))

(defmacro erl-spawn (&rest body)
  "Spawn a new process and run it in the current dynamic environment."
  `(erl-spawn-fun (lambda () ,@body) t))

(defmacro erl-spawn-link-async (&rest body)
  "Same as `erl-spawn-async', but links with the process."
  `(erl-spawn-fun (lambda () ,@body) nil t))

(defmacro erl-spawn-link (&rest body)
  "Same as `erl-spawn', but links with the process."
  `(erl-spawn-fun (lambda () ,@body) t t))

(defun erl-link (pid)
  "Link the current process with PID."
  (unless (equal pid erl-self)
    (erl-add-link erl-self pid)
    (erl-add-link pid erl-self)))

(defun erl-unlink (pid)
  "Unlink the current process from PID."
  (erl-remove-link erl-self pid)
  (erl-remove-link pid erl-self))

(defun erl-send (who message)
  "Send the term MESSAGE to the process WHO.
WHO can be a pid, a registered name (symbol), or a tuple of
 [tuple REGNAME NODENAME]."
  (cond ((erl-null-pid-p who)
	 (erl-lose-msg message))
	((erl-local-pid-p who)
	 (when (erl-local-pid-alive-p who)
	   (erl-deliver-message who message)))
	((erl-remote-pid-p who)
	 (erl-dist-send who message))
	((symbolp who)
	 (let ((proc (erl-whereis who)))
	   (if proc
	       (erl-send proc message)
	     (erl-exit (tuple 'badarg (tuple 'not-registered who))))))
	((tuplep who)			; [tuple NAME NODE]
	 (erl-dist-reg-send (elt who 2) (elt who 1) message))
	(t
	 (error "Bad pid: %S" who))))

(defun erl-exit (why)
  "Exit the current process.
Like the erlang BIF exit/1."
  (signal 'erl-exit-signal (list why)))

(defun erl-exit/2 (who why)
  "Exit a process.
Like the erlang BIF exit/2."
  (erl-send-exit erl-self who why))

(defun erl-continue (k &rest args)
  "Yield control and arrange for K to be called with ARGS the next
time this process is scheduled. Note that the process is not
\"scheduled out\" automatically, and the caller should return
normally."
  (setq erl-continuation k)
  (setq erl-continuation-args args))

(defun erl-reschedule ()
  "Return immedaitely (via `throw') to the scheduler.
Also makes the current process immediately reschedulable."
  ;; the scheduler loop will catch this and know what to do
  (throw 'schedule-out 'reschedule))

;; receive

(defmacro erl-receive (vars clauses &rest after)
  "Receive a message, matched by pattern.
If the mailbox contains a matching message, the pattern's body is
executed immediately. Otherwise, `erl-continue' is used to make the
process continue matching when new messages arrive. The crucial
difference from Erlang's receive is that erl-receive returns
immediately when nothing is matched, but will automatically resume
after a new message arrives and the process is rescheduled.

Since the the process may return and be rescheduled before the
matching message is received, the clause's body might not be executed
in the original dynamic environment. Consequently, any local variable
bindings that need to be preserved should be named in VARS.

After a pattern has been matched and executed, the AFTER forms are
then executed.

The overall syntax for receive is:

  (erl-receive (VAR-NAME ...)
      ((PATTERN . BODY)
       ...)
    . AFTER)

The pattern syntax is the same as `pmatch'."
  `(erl-start-receive (capture-bindings ,@vars)
		      ,(mcase-parse-clauses clauses)
		      (lambda () ,@after)))

(defun erl-start-receive (bs clauses after)
  ;; Setup a continuation and immediately return to the scheduler
  ;; loop, which will call us back.
  (when (equal erl-self erl-null-pid)
    (error "No process context for erl-receive"))
  (erl-continue #'erl-receive* bs clauses after)
  (erl-reschedule))

(defun erl-receive* (bs clauses after)
  (erl-receive-loop bs clauses after erl-mailbox))

(defun erl-receive-loop (bs clauses after msgs &optional acc)
  (if (null msgs) 
      (erl-continue #'erl-receive* bs clauses after)
    (let ((action
	   ;; We restore the bindings incase they are referred to in patterns
	   (with-bindings bs
	     (mcase-choose (car msgs) clauses))))
      (if (null action)
	  (erl-receive-loop bs clauses after (cdr msgs) (cons (car msgs) acc))
	(setq erl-mailbox (append (reverse acc) (cdr msgs)))
	(with-bindings bs
	  (funcall action)
	  (funcall after))))))

(defun erl-register (name &optional process)
  "Register PROCESS with NAME."
  (if (get-buffer (regname-to-bufname name))
      (erl-exit (tuple 'badarg (tuple 'already-registered name)))
    (with-erl-process (or process erl-self)
      (rename-buffer (regname-to-bufname name)))))

(defun erl-whereis (name)
  "Get the PID of the process registered with NAME, or nil if the name
is unregistered."
  (let ((buf (get-buffer (regname-to-bufname name))))
    (if buf
	(with-current-buffer buf erl-self))))

(defun regname-to-bufname (name)
  (format "*reg %S*" name))

(defalias 'erl-term-to-binary #'erlext-term-to-binary)
(defalias 'erl-binary-to-term #'erlext-binary-to-term)

;; Guts
;;
;; The scheduler works without ever being explicitly called. It runs
;; when a message arrives from a remote node or when a new process is
;; spawned, and it continues to schedule processes until none are
;; runnable.
;;
;; The %ugly-variable-names are to avoid shadowing any existing
;; dynamic bindings between the caller and the process being invoked -
;; that's a bastard to debug.

(put 'erl-exit-signal
     'error-conditions
     '(error erl-exit-signal))

(defun erl-spawn-fun (%init-function &optional %run-now-p %link)
  "Spawn a new erl process to call INIT-FUNCTION.
If RUN-NOW-P is true, the process is called immediately with the
current dynamic environment/bindings. Otherwise, the process is made
schedulable. The scheduler loop is entered if we aren't being called
by it already.
If LINK is true, the process is linked before being run."
  (let* ((%pid (make-erl-local-pid))
	 (%buffer (get-buffer-create (erl-pid-buffer-name %pid)))
	 (%gl (or erl-group-leader erl-default-group-leader)))
    (with-current-buffer %buffer
      (setq erl-self %pid)
      (setq erl-group-leader %gl)
      (setq erl-continuation %init-function)
      (setq erl-continuation-args nil)
      (erl-enroll-process))
    (when %link (erl-link %pid))
    (if %run-now-p
	(let ((erl-in-scheduler-loop t))
	  (erl-run-process %pid))
      (erl-make-schedulable %pid))
    (erl-maybe-schedule)
    %pid))

(defun erl-deliver-message (pid message)
  "Deliver MESSAGE to the mailbox of the local process PID.
Invokes the scheduler if necessary."
  (with-erl-process pid
    (setq erl-mailbox (append erl-mailbox (list message))))
  (erl-make-schedulable pid)
  (erl-maybe-schedule))

(defun erl-schedule ()
  "Enter scheduler loop until no process is runnable."
  (interactive)
  (let ((erl-in-scheduler-loop t))
    (while (erl-schedule-once)))
  ;; post-condition
  (assert (null erl-schedulable-processes)))

(defun erl-schedule-once ()
  "Schedule the next process to run. Returns true if a process was scheduled."
  (when erl-schedulable-processes
    (erl-schedule-process (pop erl-schedulable-processes))
    t))

(defun erl-maybe-schedule ()
  "Schedule processes, unless the scheduler is already running."
  (unless erl-in-scheduler-loop
    (erl-schedule)))

(defun erl-schedule-process (%pid)
  (cond ((not (erl-local-pid-alive-p %pid))
	 (message "STRANGE: %S scheduled but dead; removing" %pid)
	 (erl-make-unschedulable %pid))
	((with-erl-process %pid (null erl-continuation))
	 (message "STRANGE: %S is a zombie! killing" %pid)
	 (with-erl-process %pid (erl-terminate 'zombie)))
	(t
	 (while (eq 'reschedule (erl-run-process %pid))))))

(defun erl-run-process (%pid)
  "Run a process.
Calls the current continuation from within the process' buffer."
  (with-erl-process %pid
    ;; The %ugly-names are to avoid shadowing the caller's dynamic
    ;; bindings.
    (let ((%k erl-continuation)
	  (%args erl-continuation-args)
	  (%buffer (current-buffer)))
      (setq erl-continuation nil)
      (setq erl-continuation-args nil)
      (condition-case data
	  (progn
	    ;; if they (throw 'schedule-out value), we return the
	    ;; value, otherwise nil
	    (prog1 (catch 'schedule-out (prog1 nil (apply %k %args)))
	      (unless erl-continuation ; don't have a next continuation?
		(erl-terminate 'normal))))
	(erl-exit-signal (erl-terminate (cadr data)))
	;; FIXME: For now, any error causes the process to terminate
	;; and scheduling to continue. Is this best?
	(error           (erl-terminate `[tuple emacs-error ,data]))))))

(defun erl-make-schedulable (pid)
  "Add PID to the list of runnable processes, so that it will execute
during the next `erl-schedule'."
  (unless (member pid erl-schedulable-processes)
    (setq erl-schedulable-processes
	  (append erl-schedulable-processes (list pid)))))

(defun erl-make-unschedulable (pid)
  "Remove PID from the list of schedulable processes."
  (setq erl-schedulable-processes
	(remove pid erl-schedulable-processes)))

(defun erl-terminate (why)
  "Exit the current process."
  (unless (eq why 'normal)
    (message "EXIT: %S %S" erl-self why))
  (setq erl-exit-reason why)
  (erl-make-unschedulable erl-self)
  (kill-buffer (erl-pid->buffer erl-self)))

(defun erl-add-link (from to)
  "Unidirectionally add a link."
  (unless (erl-null-pid-p from)
    (with-erl-process from
      (add-to-list 'erl-links to))))

(defun erl-remove-link (from to)
  "Unidirectionally remove a link."
  (unless (erl-null-pid-p from)
    (with-erl-process from
      (setq erl-links (remove to erl-links)))))

;; PID utilities

(defun erl-pid-buffer-name (pid)
  (unless (equal (erl-pid-node pid) erl-node-name)
    (error "Not a local pid: %S" pid))
  (format "*pid <%S.%S.%S>*"
	  0
	  (erl-pid-id pid)
	  (erl-pid-serial pid)))

(defun erl-pid->buffer (pid)
  "Get PID's buffer."
  (or (cdr (assoc (erl-pid-id pid) erl-process-buffer-alist))
      (error "No buffer for pid %S" pid)))

(defun erl-null-pid-p (p)
  (equal p erl-null-pid))

(defun erl-local-pid-alive-p (pid)
  "Is PID a live local process?"
  (when (erl-local-pid-p pid)
    (let ((buffer (cdr (assoc (erl-pid-id pid) erl-process-buffer-alist))))
      (and buffer
	   (buffer-live-p buffer)
	   (with-erl-process pid
	     (null erl-exit-reason))))))

(defun erl-local-pid-p (x)
  "True iff X is the pid of a local process."
  (and (erl-pid-p x)
       (equal (erl-pid-node x) erl-node-name)))

(defun erl-remote-pid-p (x)
  "True iff X is the pid of a remote process."
  (and (erl-pid-p x)
       (not (erl-local-pid-p x))))

(defun erl-lose-msg (msg)
  "Log and discard a message sent to the null process."
  (with-current-buffer (get-buffer-create "*erl-lost-msgs*")
    (goto-char (point-max))
    (insert (format "%S\n" msg))))

(defun erl-enroll-process ()
  "Setup pid->buffer mapping state for the current process."
  (push (cons (erl-pid-id erl-self) (current-buffer))
	erl-process-buffer-alist)
  (make-local-variable 'kill-buffer-hook)
  (put 'kill-buffer-hook 'permanent-local t)  
  (add-hook 'kill-buffer-hook #'erl-unenroll-process)
  (add-hook 'kill-buffer-hook #'erl-propagate-exit))

(defun erl-unenroll-process ()
  (setq erl-process-buffer-alist
	(remove-if #'(lambda (x) (eq (erl-pid-id erl-self) (car x)))
		   erl-process-buffer-alist)))

(defun erl-propagate-exit ()
  (when (null erl-exit-reason)
    (setq erl-exit-reason 'killed))
  (unless (eq erl-exit-reason 'normal)
    (mapc #'(lambda (proc) (erl-send-exit erl-self proc erl-exit-reason))
	  erl-links)))

(defun erl-send-exit (from to rsn)
  (cond ((erl-local-pid-alive-p to)
	 (erl-deliver-exit from to rsn))
	((erl-remote-pid-p to)
	 (erl-dist-exit from to rsn))))

(defun erl-deliver-exit (from to rsn)
  (with-erl-process to
    (cond (erl-exit-reason	; already terminated?
	   t)
	  (erl-trap-exit
	   (erl-deliver-message to (tuple 'EXIT from rsn))
	   (erl-unlink from))
	  (t
	   (erl-terminate rsn)))))

(defun impossible (&optional reason)
  "Raise an error because something \"impossible\" has happened."
  (if reason
      (error "Impossible: %s" reason)
    (error "The impossible has occured")))

(defun nyi ()
  (error "Not yet implemented!"))

;; Initialisation

(defun erl-nodeup (node proc)
  (message "nodeup: %S" node))

(defun erl-nodedown (node)
  (message "nodedown: %S" node))

;; These hooks are defined in derl.el
(add-hook 'erl-nodeup-hook 'erl-nodeup)
(add-hook 'erl-nodedown-hook 'erl-nodedown)

;; Emacs indentation
(put 'with-erl-process 'lisp-indent-function 1)
(put 'erl-spawn 'lisp-indent-function 'defun)
(put 'erl-spawn-async 'lisp-indent-function 'defun)
(put 'erl-receive 'lisp-indent-function 2)
(put 'with-bindings 'lisp-indent-function 1)

;; Group leader

(defun erl-group-leader-loop ()
  (erl-receive ()
      (([tuple put_chars S]
	(goto-char (point-max))
	(insert s)
	(when erl-popup-on-output
	  (display-buffer (current-buffer)))))
    (erl-group-leader-loop)))

(setq erl-default-group-leader
      (erl-spawn
	(rename-buffer "*erl-output*")
	(erl-group-leader-loop)))

(provide 'erl)

