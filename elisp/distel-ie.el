;;;
;;; distel-ie - an interactive erlang shell
;;;
;;; Some of the code has shamelessly been stolen from Luke Gorrie 
;;; [luke@bluetail.com] - ripped from its elegance and replaced by bugs. 
;;; It just goes to show that you can't trust anyone these days. And
;;; as if that wasn't enough, I'll even blame Luke: "He _made_ me do it!"
;;;
;;; So, without any remorse, I hereby declare this code to be:
;;;
;;; copyright (c) 2002 david wallin [david.wallin@ul.ie].
;;;
;;; (it's probably going to be released onto an unexpecting public under
;;;  some sort of BSD license).


(require 'erlang)
;(require 'erl-service)

(make-variable-buffer-local
 (defvar erl-ie-node nil
   "Erlang node that the session is hosted on."))

(defvar erl-ie-inline-results nil
  "*Non-nil means to insert evaluation results straight into the buffer.
Nil means print them as messages.

When a prefix argument is used for session commands, the opposite
behaviour to this default is used.")

;;
;; erl-ie-session

(defun erl-ie-session (node)
  "Return the erl-ie-session for NODE, creating it if necessary."
  (interactive (list (erl-ie-read-nodename)))

  (or (get-buffer (erl-ie-buffer-name node))
      (erl-ie-create-session node)))

(defun erl-ie-create-session (node)
  (with-current-buffer (get-buffer-create (erl-ie-buffer-name node))
    (insert "%%% Welcome to the Distel Interactive Erlang Shell.\n\n")
    (push-mark (point) t)

    (erlang-mode)
    (setq erl-ie-node node)

    ;; hiijack stdin/stdout :
    (let ((output-buffer (current-buffer)))
      (setq erl-group-leader 
	    (erl-spawn (&erl-ie-group-leader-loop output-buffer))))

    (erl-ie-ensure-registered node)

    (current-buffer)))

(defun erl-ie-read-nodename ()
  "Get the node for the session, either from buffer state or from the user."
  (or erl-ie-node
      (erl-read-nodename)))

(defun erl-ie-buffer-name (node)
  (format "*ie session <%S>*" node))

;;
;; erl-ie-ensure-registered

(defun erl-ie-ensure-registered (node)
  (interactive (list (erl-ie-read-nodename)))
  (erl-spawn
    (erl-send-rpc node 'distel_ie 'ensure_registered '())))


;;
;; erl-ie-evaluate
;;
;; this is doomed to fail, can end be the smaller value ?
;; want to change to (interactive "r") somehow ...

(defun erl-ie-evaluate (start end node)
  "Evaluates a marked region. The marked region can be a function definition, a function call or an expression."
  (interactive (list 
		(region-beginning)
		(region-end)
		(erl-ie-read-nodename)))

  (let* ((string (buffer-substring-no-properties start end))

	 ;; if the current buffer isn't a session; start a session
	 ;; and copy the marked region over to it.
	 ;; all interaction will then take place in the session buffer.
	 (buffer   (if (string= (format "*ie session <%S>*" node) 
				(buffer-name (current-buffer)))

		       (current-buffer)

		     (with-current-buffer (erl-ie-session node)
		       (insert string)
		       (current-buffer)))))
    
    (erl-spawn
      (erl-send (tuple 'distel_ie node) 
		(tuple 'evaluate erl-self string))
      
      (message "Sent eval request..")

      ;; move cursor to after the marked region
      (goto-char (+ end 1))
      
      (erl-receive (buffer)
	  
	  ((['ok value]
	    (if (erl-ie-xor erl-ie-inline-results current-prefix-arg)
		;; insert directly into buffer
		(with-current-buffer buffer 
		  ;; Clear "Sent eval request.." message
		  (message "")
	      
		  ;; TODO: should check the buffer for first non-whitespace 
		  ;; before we do:
		  (newline 1)
		  (insert "--> ") (insert value) (newline 2)
		  (push-mark (point) t))
	      (display-message-or-view (format "Result: %s" value)
				       "*Evaluation Result*")))
	   
	   (['msg msg]
	    (with-current-buffer buffer
	      (message msg)))
	   
	   (['error reason]
	    (with-current-buffer buffer
	      
	      ;; TODO: should check the buffer for first non-whitespace 
	      ;; before we do:
	      (newline 1)
	      (insert "Error: ") (insert reason) (newline 1)))
	   
	   (other
	    (message "Unexpected: %S" other)))))))


(defun erl-ie-xor (a b)
  "Boolean exclusive or of A and B."
  (or (and a (not b))
      (and b (not a))))

;;
;; &erl-ie-group-leader-loop

(defun &erl-ie-group-leader-loop (buf)
  (erl-receive (buf)
      ((['put_chars s]
	(with-current-buffer buf
	  (insert s))))
    (&erl-ie-group-leader-loop buf)))


;;
;; erl-ie-show-session

(defun erl-ie-show-session (node)
  "Show the session for NODE, creating if necessary."
  (interactive (list (erl-ie-read-nodename)))
  (switch-to-buffer (erl-ie-session node)))

;;
;; erl-ie-copy-buffer-to-session

(defun erl-ie-copy-buffer-to-session (node)
  "Takes the content of the current buffer and opens a distel_ie session with it. The content is pasted at the end of the session buffer. This can be useful for debugging a file without ruining the content by mistake."
  (interactive (list (erl-ie-read-nodename)))
  (let ((cloned-buffer (buffer-string)))

    (with-current-buffer (erl-ie-session node)
      (goto-char (point-max))
      (insert cloned-buffer))
    (erl-ie-popup-buffer node)))

;;
;; erl-ie-copy-region-to-session

(defun erl-ie-copy-region-to-session (start end node)
  "Takes the content of the marked region in the current buffer and opens a distel_ie session with it. The content is pasted at the end of the session buffer. This can be useful for debugging a file without ruining the content by mistake."
  (interactive (list
		(region-beginning)
		(region-end)
		(erl-ie-read-nodename)))
  (let ((cloned-region (buffer-substring-no-properties start end)))

    (with-current-buffer (erl-ie-session node)
      (goto-char (point-max))
      (set-mark (point))		; so the region will be right
      (insert cloned-region))
    (erl-ie-popup-buffer node)))


(defun erl-ie-popup-buffer (node)
  (switch-to-buffer (erl-ie-session node)))


(provide 'distel-ie)