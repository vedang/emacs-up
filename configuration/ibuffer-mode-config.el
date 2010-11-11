;; Settings for ibuffer
; ==============================================================================

(autoload 'ibuffer "ibuffer" "List buffers." t)

(setq ibuffer-default-sorting-mode 'major-mode
      ibuffer-always-show-last-buffer t)

(setq ibuffer-saved-filter-groups
      (quote (("default"
	       ("Org"
		(mode . org-mode))
	       ("Programming"
		(or
		 (mode . c-mode)
		 (mode . c++-mode)
		 (mode . ruby-mode)
		 (mode . perl-mode)
		 (mode . python-mode)
		 (mode . emacs-lisp-mode)
                 (mode . clojure-mode)
		 ))
	       ("Documents"
		(or
		 (mode . LaTeX-mode)
		 (mode . fundamental-mode)
		 ))
	       ("ERC"   (mode . erc-mode))))))

(global-set-key (kbd "C-z") 'ibuffer-do-occur)
; ==============================================================================

(provide 'ibuffer-mode-config)
