;;; a brilliant idea:
;;; pre-defining registers
;;; (from emacs-starter-kit)

(dolist (r `((?i (file . ,(concat dotfiles-dir "init.el")))
             (?b (file . ,(concat dotfiles-dir "starter-kit-bindings.el")))
             (?r (file . ,(concat dotfiles-dir "starter-kit-registers.el")))))
  (set-register (car r) (cadr r)))

(provide 'registers)
