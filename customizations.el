; my customizations for emacs
; ===============================================================================

(setq user-full-name "Vedang Manerikar")
(setq user-mail-address "vedang.manerikar@gmail.com")
;; highlight when searching and replacing
(setq search-highlight t
      query-replace-highlight t)
;; copy-paste correctly in x-server
(setq x-select-enable-clipboard t)
(setq interprogram-paste-function 'x-cut-buffer-or-selection-value)
;; I hate that beep!
(setq visible-bell t)
(setq delete-selection-mode t)
(setq auto-save-default nil)
(setq bookmark-default-file "~/.emacs.d/bookmarks.bmk"
      bookmark-save-flag 1) ;; save my bookmarks as soon as I create them
(setq require-final-newline t)
(setq debug-on-error t)
(setq-default ispell-program-name "aspell")

;;  settings for utf8 / input-method
(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(set-language-environment "UTF-8")       ; prefer utf-8 for language settings
(set-input-method nil)                   ; no funky input for normal editing;
(setq read-quoted-char-radix 10)         ; use decimal, not octal

;; frame title: currently edited buffer name
(setq frame-title-format
      (concat "%b - " invocation-name "@" system-name))

;; always highlight the current line
(defface hl-line '((t (:background "DarkSlateBlue")))
  "Face to use for `hl-line-face'." :group 'hl-line)
(setq hl-line-face 'hl-line)
(global-hl-line-mode t)

(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))  ;; no scroll-bar
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))      ;; no tool-bar
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))      ;; no menu-bar

;; Completion ignores filenames ending in any string in this list.
(setq completion-ignored-extensions
   '(".o" ".elc" "~" ".bin" ".class" ".exe" ".ps" ".abs" ".mx" ".~jv" ".rbc"))

;; emacs push your damned backups here instead of all over the place
(push '("." . "~/.emacs.d/.emacs-backups/") backup-directory-alist)
(setq version-control t
      kept-new-versions 2
      kept-old-versions 5
      delete-old-versions t
      backup-by-copying-when-linked t)

;; if y-or-n is fine for me, it should be fine for you too!
(fset 'yes-or-no-p 'y-or-n-p)

;; delete trailing whitespace in files
(add-hook 'before-save-hook 'delete-trailing-whitespace)

(defvar programming-major-modes
  '(emacs-lisp-mode scheme-mode lisp-mode c-mode c++-mode ruby-mode conf-mode)
  "List of programming modes")

(defun prog-mode-settings ()
  "special settings for programming modes."
  (when (memq major-mode programming-major-modes)
    ;; No stray edits.Toggle with (C-x C-q) if I want to make an edit
    (toggle-read-only 1)
    ;; Flyspell mode for comments and strings
    (flyspell-prog-mode)))

(add-hook 'find-file-hook 'prog-mode-settings)

;; Indentation hook for C/C++ mode
(add-hook 'c-mode-hook 'linux-c-indent)
(add-hook 'c-mode-hook (lambda() (c-set-style "K&R")))
(add-hook 'c++-mode-hook 'linux-c-indent)

;; these are configuration files and should be opened in appropriate mode
(add-to-list 'auto-mode-alist '("\\.\\(mc\\|rc\\|def\\)$" . conf-mode))

;; customizations for auto-indentation
(defadvice yank (after indent-region activate)
  (if (member major-mode programming-major-modes)
      (let ((mark-even-if-inactive t))
        (indent-region (region-beginning) (region-end) nil))))

(defadvice yank-pop (after indent-region activate)
  (if (member major-mode programming-major-modes)
      (let ((mark-even-if-inactive t))
        (indent-region (region-beginning) (region-end) nil))))

;; settings for hippie-expand
(setq hippie-expand-try-functions-list
       '(try-expand-dabbrev
         try-expand-dabbrev-from-kill
         try-expand-dabbrev-all-buffers
         try-expand-line
         try-complete-file-name-partially
         try-complete-file-name
         try-complete-lisp-symbol-partially
         try-complete-lisp-symbol))

;; Aliases for common functions
(defalias 'qrr 'query-replace-regexp)
(defalias 'rvt 'revert-buffer)
(defalias 'dtw 'delete-trailing-whitespace)

;; Enable narrow-to-region, extremely useful for editing text
(put 'narrow-to-region 'disabled nil)

;; From EmacsWiki
;; Move to beginning of word before yanking word in isearch-mode.
;; Make C-s C-w and C-r C-w act like Vim's g* and g#, keeping Emacs'
;; C-s C-w [C-w] [C-w]... behaviour.

(require 'thingatpt)

(defun vedang/isearch-yank-word-or-char-from-beginning ()
  "Move to beginning of word before yanking word in isearch-mode."
  (interactive)
  ;; Making this work after a search string is entered by user
  ;; is too hard to do, so work only when search string is empty.
  (if (= 0 (length isearch-string))
      (beginning-of-thing 'word))
  (isearch-yank-word-or-char)
  ;; Revert to 'isearch-yank-word-or-char for subsequent calls
  (substitute-key-definition 'vedang/isearch-yank-word-or-char-from-beginning
			     'isearch-yank-word-or-char
			     isearch-mode-map))

(add-hook 'isearch-mode-hook
	  (lambda ()
	    "Activate my customized Isearch word yank command."
	    (substitute-key-definition 'isearch-yank-word-or-char
				       'vedang/isearch-yank-word-or-char-from-beginning
				       isearch-mode-map)))
; ===============================================================================

(provide 'customizations)
