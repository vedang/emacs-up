;;; Root emacs configuration file.

;;; No GUI
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;;; From nflath.com
;;; add all subdirs under "~/.emacs.d/" to load-path
(if (fboundp 'normal-top-level-add-subdirs-to-load-path)
    (let* ((my-lisp-dir "~/.emacs.d/")
           (default-directory my-lisp-dir)
           (orig-load-path load-path))
      (setq load-path (cons my-lisp-dir nil))
      (normal-top-level-add-subdirs-to-load-path)
      (nconc load-path orig-load-path)))

;;; Some global defs
(setq dotfiles-dir (file-name-directory
                    (or (buffer-file-name) load-file-name)))
(setq autoload-file (concat dotfiles-dir "loaddefs.el"))
(setq package-user-dir (concat dotfiles-dir "elpa"))
(setq custom-file (concat dotfiles-dir "custom.el"))

;;; Require common stuff
(require 'cl)
(require 'uniquify)
(require 'ansi-color)
(require 'saveplace)
(require 'paredit)

;;; Require my configuration
(require 'customizations)
(require 'utility-functions)
(require 'mode-config)
(require 'key-bindings)
(require 'registers)
(require 'misc)

(regen-autoloads)
(load custom-file 'noerror)
(totd) ; Display Tip Of The Day.

;;; init.el ends here

