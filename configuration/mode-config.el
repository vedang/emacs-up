(autoload 'package-list-packages "elpa-config" "List Elpa packages" t)
(autoload 'dired "misc-requires" "Load dired-x" t)  ; Better dired
(autoload 'magit-status "magit" "Load magit" t)
(autoload 'cscope-set-initial-directory "cscope-mode-config" "Load cscope" t)
(autoload 'twit	"twittering-mode" "" t)
(autoload 'jabber-connect "jabber-mode-config" "Load Jabber" t)
(autoload 'jabber-connect-all "jabber-mode-config" "Load Jabber" t)
(autoload 'python-mode "python-mode-config" "Load python config" t)
(autoload 'erlang-mode "erlang-mode-config" "Load erlang config" t)
(autoload 'emacs-lisp-mode "emacs-lisp-mode-config" "Load emacs lisp config" t)
(autoload 'clojure-mode "clojure-mode-config" "Load config for clojure mode" t)
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

;;; Eval after loads
(eval-after-load "org"
  '(progn
     (require 'org-mode-config)
     (org-agenda-to-appt)))
(eval-after-load "erc"
  '(require 'erc-mode-config))
(eval-after-load 'diff-mode
  '(progn
     (set-face-foreground 'diff-added "green4")
     (set-face-foreground 'diff-removed "red3")))
(eval-after-load 'magit
  '(progn
     (set-face-foreground 'magit-diff-add "green3")
     (set-face-foreground 'magit-diff-del "red3")))


;;; configuration too small to go into individual files
(require 'yasnippet) ;; not yasnippet-bundle
(global-set-key (kbd "S-TAB") 'yas/trigger-key)
(yas/initialize)
(yas/load-directory "~/.emacs.d/plugins/yasnippet/snippets")


(setq hippie-expand-try-functions-list
      '(try-expand-dabbrev
        try-expand-dabbrev-from-kill
        try-expand-dabbrev-all-buffers
        try-complete-file-name-partially
        try-complete-file-name
        try-complete-lisp-symbol-partially
        try-complete-lisp-symbol))


(setq uniquify-buffer-name-style 'reverse
      uniquify-separator "/"
      uniquify-after-kill-buffer-p t
      uniquify-ignore-buffers-re "^\\*")


(provide 'mode-config)
