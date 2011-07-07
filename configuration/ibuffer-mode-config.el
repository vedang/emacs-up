;;; Settings for ibuffer

(autoload 'ibuffer "ibuffer" "List buffers." t)
(global-set-key (kbd "C-x C-b") '(lambda ()
                                   (interactive)
                                   (ibuffer)
                                   (ibuffer-switch-to-saved-filter-groups "default")))
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
                 (mode . makefile-gmake-mode)))
               ("Org"
                (mode . org-mode))
               ("ERC"
                (mode . erc-mode))
               ("Jabber"
                (or
                 (mode . jabber-chat-mode)
                 (mode . jabber-roster-mode)))
               ("Magit"
                (mode . magit-mode))
               ("Documents"
                (or
                 (mode . LaTeX-mode)
                 (mode . fundamental-mode)))))))

(provide 'ibuffer-mode-config)
