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

;;
;; erl-ie-session

(defun erl-ie-session (node)
  "Creates an interactive shell buffer connected to a erlang node."
  (interactive (list (erl-read-nodename)))
  (let ((buf (get-buffer-create (format "*ie session <%S>*" node))))

    ;; hiijack stdin/stdout :
    (setq erl-group-leader 
	  (erl-spawn (&erl-ie-group-leader-loop buf)))

    (erl-ie-ensure-registered node)

    ;; yep, how low can you sink: global variables, jesus! :
    (with-current-buffer buf (erlang-mode))
    (set-window-buffer (selected-window) buf)

    (unless (boundp 'distel-ie-showed-welcome)
      (setq distel-ie-showed-welcome t)
      (insert (erl-ie-welcome-message)))
    buf))


;;
;; erl-ie-welcome-message

(defun erl-ie-welcome-message ()
  "%%% Welcome to the Distel Interactive Erlang Shell.\n\n")


;;
;; erl-ie-ensure-registered

(defun erl-ie-ensure-registered (node)
  (interactive (list (erl-read-nodename)))
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
		(erl-read-nodename)))

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
      
      ;; move cursor to after the marked region
      (goto-char (+ end 1))
      
      (with-current-buffer buffer (newline 1))
      
      (erl-receive (buffer)
	  
	  (([ok Value]
	    (with-current-buffer buffer 
	      
	      ;; TODO: should check the buffer for first non-whitespace 
	      ;; before we do:
	      (newline 1)
	      (insert "--> ") (insert value) (newline 1)))
	   
	   ([msg Msg]
	    (with-current-buffer buffer
	      (message msg)))
	   
	   ([error Reason]
	    (with-current-buffer buffer
	      
	      ;; TODO: should check the buffer for first non-whitespace 
	      ;; before we do:
	      (newline 1)
	      (insert "Error: ") (insert reason) (newline 1)))
	   
	   (Other
	    (message "Unexpected: %S" other)))))))


;;
;; &erl-ie-group-leader-loop

(defun &erl-ie-group-leader-loop (buf)
  (erl-receive (buf)
      (([put_chars S]
	(with-current-buffer buf
	  (insert s))))
    (&erl-ie-group-leader-loop buf)))


;;
;; erl-ie-copy-buffer-to-session

(defun erl-ie-copy-buffer-to-session (node)
  "Takes the content of the current buffer and opens a distel_ie session with it. The content is pasted at the end of the session buffer. This can be useful for debugging a file without ruining the content by mistake."
  (interactive (list (erl-read-nodename)))
  (let ((cloned-buffer (buffer-string)))

    (with-current-buffer (erl-ie-session node)
      (end-of-buffer)
      (insert cloned-buffer))))

;;
;; erl-ie-copy-region-to-session

(defun erl-ie-copy-region-to-session (start end node)
  "Takes the content of the marked region in the current buffer and opens a distel_ie session with it. The content is pasted at the end of the session buffer. This can be useful for debugging a file without ruining the content by mistake."
  (interactive (list
		(region-beginning)
		(region-end)
		(erl-read-nodename)))
  (let ((cloned-region (buffer-substring-no-properties start end)))

    (with-current-buffer (erl-ie-session node)
      (end-of-buffer)
      (insert cloned-region))))


(provide 'distel-ie)