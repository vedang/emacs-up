(autoload 'package-list-packages "elpa-config" "Load Elpa" t)
(autoload 'dired "misc-requires" "Load dired-x" t)  ; Better dired
(autoload 'magit-status "magit" "Load magit" t)
(autoload 'cscope-set-initial-directory "cscope-mode-config" "Load cscope" t)
(autoload 'org-agenda "org-mode-config" "Load org mode" t)
(autoload 'erc "erc-mode-config" "Load configuration for ERC" t)
(autoload 'twit	"twittering-mode" "" t)
(autoload 'jabber-connect "jabber-mode-config" "Load Jabber" t)
(autoload 'jabber-connect-all "jabber-mode-config" "Load Jabber" t)


;;; required magic
(require 'ido-mode-config)
(require 'ibuffer-mode-config)
(require 'latex-mode-config)
(require 'lisp-mode-config)
(require 'auto-complete-mode-config)
(require 'js2-mode-config)
(require 'revive-mode-config)
(require 'isearch-mode-config)
(require 'eshell-config)

;;; yasnippet
(require 'yasnippet) ;; not yasnippet-bundle
(global-set-key (kbd "S-TAB") 'yas/trigger-key)
(yas/initialize)
(yas/load-directory "~/.emacs.d/plugins/yasnippet/snippets")

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
