;;; a brilliant idea:
;;; pre-defining registers
;;; (from emacs-starter-kit)

(dolist (r `((?i (file . ,(concat dotfiles-dir "init.el")))))
  (set-register (car r) (cadr r)))

(provide 'registers)
