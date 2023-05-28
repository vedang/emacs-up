;;; core.el --- customizing core emacs variables
;;; Author: Vedang Manerikar
;;; Created on: 13 Oct 2013
;;; Copyright (c) 2013 Vedang Manerikar <vedang.manerikar@gmail.com>

;; This file is not part of GNU Emacs.

;;; License:

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the Do What The Fuck You Want to
;; Public License, Version 2, which is included with this distribution.
;; See the file LICENSE.txt

;;; Commentary:
;; This file contains the basic configuration needed to get started
;; with Emacs.

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
      save-place-file (concat tempfiles-dirname "places")
      column-number-mode t
      debug-on-error t
      eshell-history-file-name (concat tempfiles-dirname "eshell-history")
      eww-bookmarks-directory tempfiles-dirname
      browse-url-browser-function 'browse-url-default-browser
      bookmark-save-flag 1
      ;; Same location for bookmark file as default, but this change
      ;; makes the bookmark file independent of the .emacs.d
      ;; directory.
      bookmark-default-file (concat dotfiles-dirname "bookmarks")
      display-buffer-reuse-frames t
      whitespace-line-column 80
      recenter-positions '(top middle bottom)
      sentence-end-double-space nil
      display-time-day-and-date t
      prettify-symbols-unprettify-at-point 'right-edge
      set-mark-command-repeat-pop t
      tramp-default-method "ssh")

;; Don't clutter up directories with files
(setq backup-directory-alist
      `(("." . ,(expand-file-name
                 (concat tempfiles-dirname "backups"))))
      auto-save-list-file-prefix
      (concat tempfiles-dirname "auto-save-list/.auto-saves-")
      auto-save-file-name-transforms
      `((".*" ,(concat tempfiles-dirname "auto-save-list/") t)))

;;; Increase display length of profiler output
(with-eval-after-load 'profiler
    (setf (caar profiler-report-cpu-line-format) 80
          (caar profiler-report-memory-line-format) 80))

;;; Display garbage-collection messages, so that I can see impact on performance
(when on-my-linux-machine (setq garbage-collection-messages t))

(defvar vm/completion-ignored-extensions
  '(".exe" ".ps" ".abs" ".mx" ".~jv" ".rbc" ".beam" ".out" ".hbc")
  "Completion ignores filenames ending in any string in this list.")

(dolist (ext vm/completion-ignored-extensions)
  (add-to-list 'completion-ignored-extensions ext))

;; Set path for saving desktop
(require 'desktop)
(add-to-list 'desktop-path tempfiles-dirname)

;;; Everything in UTF8
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(when (boundp 'buffer-file-coding-system)
    (setq-default buffer-file-coding-system 'utf-8))
(when (boundp 'default-buffer-file-coding-system)
  (setq default-buffer-file-coding-system 'utf-8))
(setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING))

(setq file-name-coding-system 'utf-8)
(setq buffer-file-coding-system 'utf-8)
(setq coding-system-for-write 'utf-8)

(set-clipboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(setq default-process-coding-system '(utf-8 . utf-8))


(setq-default indent-tabs-mode nil  ;only spaces by default.
              tab-width 4)


(mouse-avoidance-mode 'exile)
(delete-selection-mode t)
(display-time)


;;; hooks
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(add-hook 'text-mode-hook 'turn-on-visual-line-mode)

(add-to-list 'safe-local-variable-values '(lexical-binding . t))

;; Enable narrow-to-region, extremely useful for editing text
(put 'narrow-to-region 'disabled nil)

;; Zone
(require 'zone)
(zone-when-idle 300)

;; Tree-sitter
(require 'treesit)
(setq treesit-language-source-alist
      '((bash "https://github.com/tree-sitter/tree-sitter-bash")
        (c "https://github.com/tree-sitter/tree-sitter-c")
        (cpp "https://github.com/tree-sitter/tree-sitter-cpp")
        (clojure "https://github.com/sogaiu/tree-sitter-clojure")
        (cmake "https://github.com/uyha/tree-sitter-cmake")
        (css "https://github.com/tree-sitter/tree-sitter-css")
        (elisp "https://github.com/Wilfred/tree-sitter-elisp")
        (html "https://github.com/tree-sitter/tree-sitter-html")
        (java "https://github.com/tree-sitter/tree-sitter-java")
        (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
        (json "https://github.com/tree-sitter/tree-sitter-json")
        (make "https://github.com/alemuller/tree-sitter-make")
        (markdown "https://github.com/ikatyang/tree-sitter-markdown")
        (python "https://github.com/tree-sitter/tree-sitter-python")
        (rust "https://github.com/tree-sitter/tree-sitter-rust")
        (toml "https://github.com/tree-sitter/tree-sitter-toml")
        (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
        (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
        (yaml "https://github.com/ikatyang/tree-sitter-yaml")))

;;; Open these files in the appropriate mode
(add-to-list 'auto-mode-alist '("\\.\\(mc\\|rc\\|def\\)$" . conf-mode))
(add-to-list 'auto-mode-alist '("\\.\\(erl\\|hrl\\)$" . erlang-mode))
(add-to-list 'auto-mode-alist '("\\.\\(tex\\|ltx\\)$" . LaTeX-mode))
(add-to-list 'auto-mode-alist '("Vagrantfile$" . ruby-mode))
(if (eq system-type 'darwin)
    (add-to-list 'auto-mode-alist '("\\.m$" . objc-mode))
  (add-to-list 'auto-mode-alist '("\\.m$" . octave-mode)))

(when (treesit-ready-p 'bash)
  (add-to-list 'auto-mode-alist '("\\.\\(sh\\|bash\\)$" . bash-ts-mode)))
(when (treesit-ready-p 'c)
  (add-to-list 'auto-mode-alist '("\\.\\(c\\|h\\)$" . c-ts-mode)))
(when (treesit-ready-p 'cpp)
  (add-to-list 'auto-mode-alist '("\\.\\(cpp\\|hpp\\)$" . cpp-ts-mode)))
;; Since this is my bread and butter language, I won't move to
;; tree-sitter until I'm happy with the status of the work.

;; (when (treesit-ready-p 'clojure)
;;   (add-to-list 'auto-mode-alist '("\\.\\(clj\\|cljc\\|cljs\\)$" . clojure-ts-mode)))
(when (treesit-ready-p 'cmake)
  (add-to-list 'auto-mode-alist
               '("\\(?:CMakeLists\\.txt\\|\\.cmake\\)\\'" . cmake-ts-mode)))
(when (treesit-ready-p 'css)
  (add-to-list 'auto-mode-alist '("\\.css\\'" . css-ts-mode)))
(when (treesit-ready-p 'html)
  (add-to-list 'auto-mode-alist '("\\.\\(html\\|xhtml\\)$" . html-ts-mode)))
(when (treesit-ready-p 'java)
  (add-to-list 'auto-mode-alist '("\\.java\\'" . java-ts-mode)))
(when (treesit-ready-p 'javascript)
  (add-to-list 'auto-mode-alist '("\\(\\.js[mx]\\|\\.har\\)\\'" . js-ts-mode)))
(when (treesit-ready-p 'json)
  (add-to-list 'auto-mode-alist '("\\.json\\'" . json-ts-mode)))
(when (treesit-ready-p 'make)
  ;; nothing to do, there is no make-ts-mode yet.
  )
(when (treesit-ready-p 'markdown)
  ;; nothing to do, there is no markdown-ts-mode yet.
  )
(when (treesit-ready-p 'python)
  (add-to-list 'auto-mode-alist '("\\.py[iw]?\\'" . python-ts-mode))
  (add-to-list 'interpreter-mode-alist '("python[0-9.]*" . python-ts-mode)))
(when (treesit-ready-p 'rust)
  (add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-ts-mode)))
(when (treesit-ready-p 'toml)
  (add-to-list 'auto-mode-alist '("\\.toml\\'" . toml-ts-mode)))
(when (treesit-ready-p 'tsx)
  (add-to-list 'auto-mode-alist '("\\.tsx\\'" . tsx-ts-mode)))
(when (treesit-ready-p 'typescript)
  (add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-ts-mode)))
(when (treesit-ready-p 'yaml)
  (add-to-list 'auto-mode-alist '("\\.ya?ml\\'" . yaml-ts-mode)))

(defun mp-remove-treesit-sexp-changes ()
  (when (eq forward-sexp-function #'treesit-forward-sexp)
    (setq forward-sexp-function nil))
  (when (eq transpose-sexps-function #'treesit-transpose-sexps)
    (setq transpose-sexps-function #'transpose-sexps-default-function))
  (when (eq forward-sentence-function #'treesit-forward-sentence)
    (setq forward-sentence-function #'forward-sentence-default-function)))

(add-hook 'prog-mode-hook #'mp-remove-treesit-sexp-changes)

(provide 'core)
;;; core.el ends here
