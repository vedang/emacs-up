;;; Settings for ibuffer

(autoload 'ibuffer "ibuffer" "List buffers." t)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "C-z") 'ibuffer-do-occur)

(setq ibuffer-default-sorting-mode 'major-mode
      ibuffer-always-show-last-buffer t)

(setq ibuffer-saved-filter-groups
      (quote (("default"
               ("Programming"
                (or
                 (mode . c-mode)
                 (mode . c++-mode)
                 (mode . erlang-mode)
                 (mode . perl-mode)
                 (mode . python-mode)
                 (mode . emacs-lisp-mode)
                 (mode . clojure-mode)
                 ))
               ("Org"
                (mode . org-mode))
               ("Documents"
                (or
                 (mode . LaTeX-mode)
                 (mode . fundamental-mode)
                 ))
               ("ERC"   (mode . erc-mode))))))

(provide 'ibuffer-mode-config)
