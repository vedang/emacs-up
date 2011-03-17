;;; a brilliant idea:
;;; pre-defining registers
;;; (from emacs-starter-kit)

(dolist (r `((?i (file . ,(concat dotfiles-dir "init.el")))
             (?o (file . ,(concat dotfiles-dir "configuration/org-mode-config.el")))))
  (set-register (car r) (cadr r)))

(provide 'registers)
