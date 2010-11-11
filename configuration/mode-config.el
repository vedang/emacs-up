;;; Load ELPA
(require 'package)
(package-initialize)
(require 'elpa-config)

;;; Better dired
(require 'dired-x)

;;; cscope - awesome search in c/c++ files
(require 'xcscope)
(require 'cscope-mode-config)

;;; ido magic
(require 'ido-mode-config)

;;; ibuffer magic
(require 'ibuffer-mode-config)

;;; yasnippet
(require 'yasnippet) ;; not yasnippet-bundle
(global-set-key (kbd "S-TAB") 'yas/trigger-key)
(yas/initialize)
(yas/load-directory "~/.emacs.d/plugins/yasnippet/snippets")

;;; ERC
(require 'erc-mode-config)

;;; LaTeX-mode
(require 'latex-mode-config)

;;; emacs-lisp mode
(require 'emacs-lisp-mode-config)

;;; org-mode
(require 'org-install)
(require 'org-mode-config)
(org-agenda-to-appt) ;; Load appointments for today

;;; auto-complete
(require 'auto-complete-config)
(require 'auto-complete-mode-config)

;;; JS
(require 'js2-mode-config)

;;; twitter
(autoload 'twit	"twittering-mode" "" t)

;;; revive mode
(require 'revive-mode-config)

;;; jabber
(require 'jabber-autoloads)
(require 'jabber-mode-config)

;;; isearch-mode tweak
(require 'isearch-mode-config)

;;; smart-tab
(require 'smart-tab)
(global-smart-tab-mode 1)

;;; eshell
(require 'eshell-config)

(provide 'mode-config)
