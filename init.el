(setq custom-file "~/.emacs.d/emacs-custom.el")
(load custom-file)

;;adds all subdirs under .emacs.d to load-path (nflath.com)
(if (fboundp 'normal-top-level-add-subdirs-to-load-path)
        (let* ((my-lisp-dir "~/.emacs.d/")
              (default-directory my-lisp-dir)
	      (orig-load-path load-path))
	  (setq load-path (cons my-lisp-dir nil))
          (normal-top-level-add-subdirs-to-load-path)
	  (nconc load-path orig-load-path)))

(require 'customizations)
(require 'utility-functions)
(require 'mode-configs)
(require 'key-bindings)

;;Someday, I'll sit down and set it up so that I can use the server-client feature properly
;(require 'server)
;(when (and (= emacs-major-version 23) (equal window-system 'w32))
;  (defun server-ensure-safe-dir (dir) "Noop" t)) ; Suppress error "directory
					; ~/.emacs.d/server is unsafe"
					; on windows.
;(server-start)

;;Print Tip of the Day
(totd)

;;; This was installed by package-install.el.
;;; This provides support for the package system and
;;; interfacing with ELPA, the package archive.
;;; Move this code earlier if you want to reference
;;; packages in your .emacs.
(when
    (load
     (expand-file-name "~/.emacs.d/elpa/package.el"))
  (package-initialize))

