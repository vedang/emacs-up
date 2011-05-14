(autoload 'package-list-packages "elpa-config" "Load Elpa" t)
(autoload 'dired "misc-requires" "Load dired-x" t)  ; Better dired

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
;(require 'erc-mode-config)

;;; LaTeX-mode
(require 'latex-mode-config)

;;; lisp modes
(require 'lisp-mode-config)

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
