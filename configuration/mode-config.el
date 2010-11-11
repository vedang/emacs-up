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
