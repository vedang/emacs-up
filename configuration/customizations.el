;;; customizations.el --- my customizations for emacs
;;; Author: Vedang Manerikar
;;; Created on: 08 Jan 2012
;;; Time-stamp: "2012-08-31 20:41:37 vedang"
;;; Copyright (c) 2012 Vedang Manerikar <vedang.manerikar@gmail.com>

;; This file is not part of GNU Emacs.

;;; License:

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the Do What The Fuck You Want to
;; Public License, Version 2, which is included with this distribution.
;; See the file LICENSE.txt

;;; Code:


(setq user-full-name "Vedang Manerikar"
      user-mail-address "vedang.manerikar@gmail.com"
      message-log-max t
      visible-bell t
      echo-keystrokes 0.1
      inhibit-startup-message t
      font-lock-maximum-decoration t
      confirm-kill-emacs 'y-or-n-p
      require-final-newline t
      ediff-window-setup-function 'ediff-setup-windows-plain
      save-place-file (concat *tempfiles-dir* "places")
      column-number-mode t
      debug-on-error t
      bookmark-default-file (concat *tempfiles-dir* "bookmarks.bmk")
      bookmark-save-flag 1
      display-buffer-reuse-frames t
      whitespace-line-column 80
      flyspell-issue-welcome-flag nil
      display-time-day-and-date t)


(mouse-avoidance-mode 'banish)
(delete-selection-mode t)
(show-paren-mode 1)
(display-time)
(setq-default indent-tabs-mode nil  ;only spaces by default.
              tab-width 4)


(defalias 'yes-or-no-p 'y-or-n-p)


;; Don't clutter up directories with files~
(setq backup-directory-alist
      `(("." . ,(expand-file-name
                 (concat *tempfiles-dir* "backups"))))
      auto-save-list-file-prefix
      (concat *tempfiles-dir* "auto-save-list/.auto-saves-")
      auto-save-file-name-transforms
      `((".*" ,(concat *tempfiles-dir* "auto-save-list/") t)))

(add-to-list 'safe-local-variable-values '(lexical-binding . t))

;; Completion ignores filenames ending in any string in this list.
(setq completion-ignored-extensions
      '(".o" ".elc" "~" ".bin" ".class" ".exe" ".ps" ".abs" ".mx"
        ".~jv" ".rbc" ".pyc" ".beam" ".aux" ".out" ".pdf"))


;;; hooks
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(add-hook 'before-save-hook 'time-stamp)
;; This function comes from revive-mode
(add-hook 'kill-emacs-hook 'emacs-save-layout)
(add-hook 'find-file-hook 'flymake-find-file-hook)
(add-hook 'text-mode-hook 'turn-on-visual-line-mode)
(add-hook 'text-mode-hook 'turn-on-flyspell)
(add-hook 'fundamental-mode-hook 'turn-on-flyspell)


;;; open these files in the appropriate mode
(add-to-list 'auto-mode-alist '("\\.\\(mc\\|rc\\|def\\)$" . conf-mode))
(add-to-list 'auto-mode-alist '("\\.\\(erl\\|hrl\\)$" . erlang-mode))
(add-to-list 'auto-mode-alist '("\\.\\(tex\\|ltx\\)$" . LaTeX-mode))
(add-to-list 'auto-mode-alist '("\\.markdown$" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.m$" . octave-mode))


;; customizations for auto-indentation
(defadvice yank (after indent-region activate)
  (if (member major-mode vedang/programming-major-modes)
      (let ((mark-even-if-inactive t))
        (indent-region (region-beginning) (region-end) nil))))


(defadvice yank-pop (after indent-region activate)
  (if (member major-mode vedang/programming-major-modes)
      (let ((mark-even-if-inactive t))
        (indent-region (region-beginning) (region-end) nil))))


;; Enable narrow-to-region, extremely useful for editing text
(put 'narrow-to-region 'disabled nil)


;; load this color-theme
(require 'color-theme)
(require 'color-theme-billw)
(color-theme-billw)
(setq color-theme-is-global t)


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


;; Close emacsclient buffers using C-x k
(add-hook 'server-switch-hook
          (lambda ()
            (when (current-local-map)
              (use-local-map (copy-keymap (current-local-map))))
            (when server-buffer-clients
              (local-set-key (kbd "C-x k") 'server-edit))))


(provide 'customizations)
