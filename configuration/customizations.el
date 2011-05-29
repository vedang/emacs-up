;;; my customizations for emacs

(setq user-full-name "Vedang Manerikar"
      user-mail-address "vedang.manerikar@gmail.com"
      message-log-max t                 ; Log as much as possible on startup
      visible-bell t
      echo-keystrokes 0.1
      inhibit-startup-message t
      font-lock-maximum-decoration t
      mouse-avoidance-mode 'banish
      confirm-kill-emacs 'y-or-n-p
      transient-mark-mode t
      color-theme-is-global t
      require-final-newline t
      ediff-window-setup-function 'ediff-setup-windows-plain
      save-place-file (concat tempfiles-dir "places")
      x-select-enable-clipboard t
      column-number-mode t
      delete-selection-mode t
      debug-on-error t
      bookmark-default-file (concat tempfiles-dir "bookmarks.bmk")
      bookmark-save-flag 1     ; Save bookmarks as soon as I create them
      display-buffer-reuse-frames t     ; Useful when dealing with REPLs
      auto-compression-mode t)

(show-paren-mode 1)

(setq-default indent-tabs-mode nil  ;only spaces by default.
              tab-width 4
              ispell-program-name "aspell")

(defalias 'yes-or-no-p 'y-or-n-p)


;; Don't clutter up directories with files~
(setq backup-directory-alist
      `(("." . ,(expand-file-name
                 (concat tempfiles-dir "backups"))))
      auto-save-list-file-prefix
      (concat tempfiles-dir "auto-save-list/.auto-saves-")
      auto-save-file-name-transforms
      `((".*" ,(concat tempfiles-dir "auto-save-list/") t)))

(add-to-list 'safe-local-variable-values '(lexical-binding . t))1
(add-to-list 'safe-local-variable-values '(whitespace-line-column . 80))

;; Completion ignores filenames ending in any string in this list.
(setq completion-ignored-extensions
      '(".o" ".elc" "~" ".bin" ".class" ".exe" ".ps" ".abs" ".mx"
        ".~jv" ".rbc" ".pyc" ".beam" ".aux" ".out" ".pdf"))


;;; hooks
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(add-hook 'find-file-hook 'flymake-find-file-hook)
(add-hook 'text-mode-hook 'turn-on-visual-line-mode)
(add-hook 'text-mode-hook 'turn-on-flyspell)
(add-hook 'lisp-mode-hook 'turn-on-paredit)


(defvar programming-major-modes
  '(js2-mode c-mode c++-mode conf-mode clojure-mode erlang-mode
             emacs-lisp-mode lisp-mode scheme-mode python-mode)
  "List of programming modes")

(defun vedang/prog-mode-settings ()
  "special settings for programming modes."
  (when (memq major-mode programming-major-modes)
    ;; No stray edits.Toggle with (C-x C-q) if I want to make an edit
    (when (not (eq major-mode 'emacs-lisp-mode))
      (toggle-read-only 1))
    ;; Flyspell mode for comments and strings
    (flyspell-prog-mode)
    ;; tell me if lines exceed 80 columns
    (turn-on-whitespace-mode)
    ;; paredit should always be there, except in python
    (when (not (eq major-mode 'python-mode))
      (turn-on-paredit))
    (run-coding-hook)))
(add-hook 'find-file-hook 'vedang/prog-mode-settings)


;; Indentation hook for C/C++ mode
;; As defined in Documentation/CodingStyle
(defun vedang/linux-c-indent ()
  "adjusted defaults for C/C++ mode use with the Linux kernel."
  (interactive)
  (setq tab-width 8)
  (setq indent-tabs-mode nil) ;; force spaces, to work with dumber editors
  (setq c-basic-offset 8))
(add-hook 'c-mode-hook 'vedang/linux-c-indent)
(add-hook 'c-mode-hook (lambda() (c-set-style "K&R")))
(add-hook 'c++-mode-hook 'vedang/linux-c-indent)


;;; open these files in the appropriate mode
(add-to-list 'auto-mode-alist '("\\.\\(mc\\|rc\\|def\\)$" . conf-mode))
(add-to-list 'auto-mode-alist '("\\.\\(erl\\|hrl\\)$" . erlang-mode))
(add-to-list 'auto-mode-alist '("\\.clj$" . clojure-mode))
(add-to-list 'auto-mode-alist '("\\.\\(tex\\|ltx\\)$" . LaTeX-mode))

;; customizations for auto-indentation
(defadvice yank (after indent-region activate)
  (if (member major-mode programming-major-modes)
      (let ((mark-even-if-inactive t))
        (indent-region (region-beginning) (region-end) nil))))

(defadvice yank-pop (after indent-region activate)
  (if (member major-mode programming-major-modes)
      (let ((mark-even-if-inactive t))
        (indent-region (region-beginning) (region-end) nil))))


;; Enable narrow-to-region, extremely useful for editing text
(put 'narrow-to-region 'disabled nil)

;; load this color-theme
(require 'color-theme)
(require 'color-theme-billw)
(color-theme-billw)


;; when I create a temporary buffer, it should auto-detect the right
;; mode to start in the buffer
(setq default-major-mode (lambda ()
                           (let ((buffer-file-name (or buffer-file-name
                                                       (buffer-name))))
                             (set-auto-mode))))

;;; Everything in UTF8
(prefer-coding-system 'utf-8)
(set-language-environment 'UTF-8)
(set-default-coding-systems 'utf-8)
(setq file-name-coding-system 'utf-8)
(setq buffer-file-coding-system 'utf-8)
(setq coding-system-for-write 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-clipboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(setq default-process-coding-system '(utf-8 . utf-8))


(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "google-chrome")


(provide 'customizations)
