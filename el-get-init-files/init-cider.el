;;; init-cider.el --- Configuration for Cider. -*- lexical-binding: t -*-
;;; Commentary:
;;; Author: Vedang Manerikar
;;; Created on: 27 Oct 2013
;;; Copyright (c) 2013 Vedang Manerikar <vedang.manerikar@gmail.com>

;; This file is not part of GNU Emacs.

;;; License:

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the Do What The Fuck You Want to
;; Public License, Version 2, which is included with this distribution.
;; See the file LICENSE.txt

;;; Code:

(defun cider-repl-prompt-on-newline (ns)
  "Return a prompt string with newline.
NS is the namespace information passed into the function by cider."
  (concat ns ">\n"))

(defun vineet/cider-load-open-buffers ()
  "Load all open clojure buffers in the project."
  (interactive)
  (dolist (buf (project-root (project-current t)))
    (let ((buf-file-path (buffer-file-name buf)))
      (when (and buf-file-path
                 (string= (file-name-extension buf-file-path) "clj")
                 (not (string= (file-name-nondirectory buf-file-path)
                               "project.clj")))
        (cider-load-buffer buf)))))

(with-eval-after-load 'cider-mode
  (add-hook 'cider-mode-hook 'eldoc-mode)
  (define-key cider-mode-map (kbd "C-c z") 'cider-selector)
  (setq cider-repl-history-size most-positive-fixnum
        cider-repl-wrap-history t
        cider-repl-prompt-function 'cider-repl-prompt-on-newline
        cider-repl-use-pretty-printing nil
        nrepl-buffer-name-separator "-"
        nrepl-buffer-name-show-port t
        nrepl-log-messages t
        cider-mode-line nil
        cider-annotate-completion-candidates t
        cider-completion-annotations-include-ns 'always
        cider-show-error-buffer 'always
        cider-prompt-for-symbol nil
        cider-auto-jump-to-error 'errors-only
        cider-apropos-actions
        '(("find-def" . cider--find-var)
          ("display-doc" . cider-doc-lookup)
          ("lookup-on-clojuredocs" . cider-clojuredocs-lookup))))

(with-eval-after-load 'cider-repl
  (add-hook 'cider-repl-mode-hook 'subword-mode)
  (define-key cider-repl-mode-map (kbd "C-M-q") 'prog-indent-sexp)
  (define-key cider-repl-mode-map (kbd "C-c M-o") 'cider-repl-clear-buffer))

(defun clerk-show ()
  "Show the current-file using Clerk."
  (interactive)
  (when-let ((filename (buffer-file-name)))
    (save-buffer)
    (cider-interactive-eval
     (concat "(nextjournal.clerk/show! \"" filename "\")"))))

(with-eval-after-load 'clojure-mode
  (define-key clojure-mode-map (kbd "<M-return>") 'clerk-show))

(provide 'init-cider)
;;; init-cider ends here
