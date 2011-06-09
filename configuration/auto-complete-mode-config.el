;;; my settings for ac-mode

(require 'auto-complete-config)

(add-to-list 'ac-dictionary-directories
             "~/.emacs.d/plugins/auto-complete/ac-dict")

(ac-config-default) ; use the default sources, they are good
(setq ac-auto-show-menu 0.8) ; Show auto-complete menu 0.8 second later

(setq ac-use-menu-map t)
(setq ac-comphist-file (concat tempfiles-dir "ac-comphist.dat"))
(define-key ac-menu-map "\C-n" 'ac-next)
(define-key ac-menu-map "\C-p" 'ac-previous)

(provide 'auto-complete-mode-config)
