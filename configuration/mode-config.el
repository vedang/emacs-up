(autoload 'package-list-packages "elpa-config" "Load Elpa" t)
(autoload 'dired "misc-requires" "Load dired-x" t)  ; Better dired
(autoload 'magit-status "magit" "Load magit" t)
(autoload 'cscope-set-initial-directory "cscope-mode-config" "Load cscope" t)
(autoload 'org-agenda "org-mode-config" "Load org mode" t)

;;; required magic
(require 'ido-mode-config)
(require 'ibuffer-mode-config)
(require 'latex-mode-config)
(require 'lisp-mode-config)

;;; yasnippet
(require 'yasnippet) ;; not yasnippet-bundle
(global-set-key (kbd "S-TAB") 'yas/trigger-key)
(yas/initialize)
(yas/load-directory "~/.emacs.d/plugins/yasnippet/snippets")

;;; ERC
;(require 'erc-mode-config)

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

;;; eshell
(require 'eshell-config)

;;; erlang
(require 'erlang-start)
(require 'distel)
(distel-setup)
(require 'erlang-mode-config)

;;; python
(require 'python)
(require 'python-mode-config)

;;; flymake
(require 'flymake-config)

(provide 'mode-config)
