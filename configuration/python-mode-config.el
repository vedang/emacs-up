;;; settings for python

(defun load-ropemacs ()
  "Load pymacs and ropemacs"
  (interactive)
  (require 'pymacs)
  (pymacs-load "ropemacs" "rope-")

  (setq ropemacs-confirm-saving 'nil)
  (setq ropemacs-enable-autoimport t))
(add-hook 'python-mode-hook 'load-ropemacs)

(setq python-check-command "pyflakes")

(provide 'python-mode-config)
