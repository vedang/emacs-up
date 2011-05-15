(autoload 'package-list-packages "elpa-config" "Load Elpa" t)
(autoload 'dired "misc-requires" "Load dired-x" t)  ; Better dired
(autoload 'magit-status "magit" "Load magit" t)
(autoload 'cscope-set-initial-directory "cscope-mode-config" "Load cscope" t)
(autoload 'org-agenda "org-mode-config" "Load org mode" t)
(autoload 'erc "erc-mode-config" "Load configuration for ERC" t)
(autoload 'twit	"twittering-mode" "" t)
(autoload 'jabber-connect "jabber-mode-config" "Load Jabber" t)
(autoload 'jabber-connect-all "jabber-mode-config" "Load Jabber" t)
(autoload 'python-mode "python-mode-config" "Load python config" t)
(autoload 'erlang-mode "erlang-mode-config" "Load erlang config" t)
(autoload 'no-easy-keys-minor-mode "no-easy-keys" "Load no easy keys" t)

;;; required magic
(require 'ido-mode-config)
(require 'ibuffer-mode-config)
(require 'latex-mode-config)
(require 'lisp-mode-config)
(require 'auto-complete-mode-config)
(require 'revive-mode-config)
(require 'isearch-mode-config)
(require 'flymake-config)
(require 'js2-mode-config)


;;; yasnippet
(require 'yasnippet) ;; not yasnippet-bundle
(global-set-key (kbd "S-TAB") 'yas/trigger-key)
(yas/initialize)
(yas/load-directory "~/.emacs.d/plugins/yasnippet/snippets")


(provide 'mode-config)
