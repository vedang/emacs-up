(require 'pymacs)
(pymacs-load "ropemacs" "rope-")
(setq ropemacs-enable-autoimport t)
(setq python-check-command "pyflakes")

(provide 'python-mode-config)
