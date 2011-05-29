;;; Root emacs configuration file.

(defvar *emacs-load-start* (current-time))

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
(setq tempfiles-dir (concat dotfiles-dir "temp-files/"))

;;; Require common stuff
(require 'cl)
(require 'uniquify)
(require 'saveplace)
(require 'paredit)
(require 'whitespace)

;;; Require my configuration
(require 'customizations)
(require 'utility-functions)
(require 'mode-config)
(require 'key-bindings)
(require 'registers)

(regen-autoloads)
(load custom-file 'noerror)
(server-start)

(message "My .emacs loaded in %ds" (destructuring-bind (hi lo ms) (current-time)
                                     (- (+ hi lo) (+ (first *emacs-load-start*) (second
                                                                                 *emacs-load-start*)))))
(totd) ; Display Tip Of The Day.

;;; init.el ends here

