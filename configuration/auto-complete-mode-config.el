;;; my settings for ac-mode

(require 'auto-complete-config)

(add-to-list 'ac-dictionary-directories
             (concat *plugins-dir* "auto-complete/ac-dict"))

(ac-config-default) ; use the default sources, they are good
(ac-flyspell-workaround) ; stop autocomplete from clashing with flyspell
(global-auto-complete-mode t) ; use autocomplete everywhere
(setq ac-auto-show-menu 0.8) ; Show auto-complete menu 0.8 second later

(set-default 'ac-sources
             '(ac-source-dictionary
               ac-source-words-in-buffer
               ac-source-words-in-same-mode-buffers
               ac-source-words-in-all-buffer))

(dolist (mode '(magit-log-edit-mode log-edit-mode org-mode text-mode html-mode
                                    nxml-mode clojure-mode lisp-mode textile-mode
                                    markdown-mode))
  (add-to-list 'ac-modes mode))

(setq ac-use-menu-map t)
(setq ac-comphist-file (concat *tempfiles-dir* "ac-comphist.dat"))
(define-key ac-menu-map "\C-n" 'ac-next)
(define-key ac-menu-map "\C-p" 'ac-previous)

(provide 'auto-complete-mode-config)
