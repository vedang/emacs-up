;Settings for things I require
;===============================================================================

(require 'dired-x)                     ;;better dired
;===============================================================================

;;uniquify settings
(require 'uniquify)
(setq uniquify-buffer-name-style 'reverse)
(setq uniquify-separator "/")
(setq uniquify-after-kill-buffer-p t)
(setq uniquify-ignore-buffers-re "^\\*")
;===============================================================================

;;cscope - awesome search in c/c++ files
(require 'xcscope)
(require 'cscope-mode-config)
;===============================================================================

;;ido - interactively do things
(require 'ido)
(require 'ido-mode-config)
;===============================================================================

;;settings for ibuffer
(require 'ibuffer-mode-config)
;===============================================================================

;;settings for shell
(require 'ansi-color)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)
(setq comint-prompt-read-only t)
;===============================================================================

;;settings for yasnippet
(require 'yasnippet) ;; not yasnippet-bundle
(global-set-key (kbd "S-TAB") 'yas/trigger-key)
(yas/initialize)
(yas/load-directory "~/.emacs.d/plugins/yasnippet/snippets")
;===============================================================================

;;settings for ERC
(require 'erc-mode-config)
;===============================================================================

;;org-mode settings (put at end of file, as it screws up in cygwin-emacs22)
(require 'org-install)
(require 'org-mode-config)
(org-agenda-to-appt) ;;Load appointments for today
;===============================================================================

;;htmlize settings (comes handy for region conversion)
(require 'htmlize)
(setq htmlize-output-type 'inline-css)
;===============================================================================

;;perl mode settings
(require 'cperl-mode)
(require 'perl-mode-config)
;===============================================================================

;;settings for LaTeX-mode
(require 'latex-mode-config)
;===============================================================================

;;settings for emacs-lisp mode
(require 'emacs-lisp-mode-config)
;===============================================================================

;;settings for my current color-theme
(require 'color-theme)
(color-theme-initialize)
(color-theme-gray30)
(set-face-attribute 'default nil :font "Consolas-12")
; ==============================================================================

;;settings for dot-mode
(autoload 'dot-mode "dot-mode" nil t) ; vi `.' command emulation
(global-set-key [(control ?.)] (lambda () (interactive) (dot-mode 1)
				 (message "Dot mode activated.")))
; ==============================================================================

;;settings for ruby mode
(require 'rinari)
(require 'ruby-mode-config)
; ==============================================================================

;; settings for nxml-mode
(require 'nxml-mode-config)
; ==============================================================================

; Make diff mode pretty
(eval-after-load 'diff-mode
  '(progn
     (set-face-foreground 'diff-added "DeepSkyBlue")
     (set-face-foreground 'diff-removed "sienna")))
; ==============================================================================

;; Nav-mode for easy navigation of directories.
(autoload 'nav "nav" nil t)
; ==============================================================================

(provide 'mode-configs)
