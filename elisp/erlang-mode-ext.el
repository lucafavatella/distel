;;; erlang-mode-ext.el --- Extensions to erlang-mode

(require 'erl)
(require 'erlang)

(defun debug-interpret (node module)
  (interactive (list (erl-read-nodename)
		     (erlang-get-module)))
  (erl-spawn
    (erl-send-rpc node 'int 'i (list module))
    (erl-receive ()
	(([tuple rex [tuple module Mod]]
	  (message "Now interpreting %S" mod))
	 ([tuple rex error]
	  (message "Failed!"))))))

(define-key erlang-mode-map "\C-c\M-:" 'erl-eval-expression)

(provide 'erlang-mode-ext)
