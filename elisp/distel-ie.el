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

;;; TODO: erl-interactive-shell should check if the distel_ie server
;;;       is up and running, otherwise start it.

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

    (with-current-buffer buf (erlang-mode))
    (set-window-buffer (selected-window) buf)

    (insert (erl-ie-welcome-message))))


;;
;; erl-ie-welcome-message

(defun erl-ie-welcome-message ()
  "%%% Welcome to the Distel Interactive Erlang Shell v0.0.1\n\n")


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
  (interactive (list 
		(region-beginning)
		(region-end)
		(erl-read-nodename)))
  (let ((string (buffer-substring-no-properties start end))
	(buffer (current-buffer)))

    (erl-spawn
      (erl-send (tuple 'distel_ie node) 
		(tuple 'evaluate erl-self string))

    ;; move cursor to after the marked region
    (goto-char (+ end 1))

    ;; should check the buffer for first non-whitespace before we do:
    (newline 2)

    (erl-receive (buffer)
	(([ok Value]
	  (with-current-buffer buffer 
	    (insert "--> ") (insert value) (newline 1)))
	 ([error Reason]
	  (with-current-buffer buffer
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


;; semantic-beginning-of-context
;; semantic-end-of-context

(provide 'distel-ie)