
;;; Automatically inserted by Distel config_install

(require 'erlang-mode-ext)

(defun distel-erlang-mode-hook ()
  (erlang-extended-mode t))

(add-hook 'erlang-mode-hook 'distel-erlang-mode-hook)

;;; End of modifications by Distel config_install

