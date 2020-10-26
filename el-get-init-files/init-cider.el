;;; init-cider.el --- Configuration for Cider.
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


(when (not (boundp 'tempfiles-dirname))
  (setq tempfiles-dirname "~/.emacs.d/"))


(defun cider-repl-prompt-on-newline (ns)
  "Return a prompt string with newline.
NS is the namespace information passed into the function by cider."
  (concat ns ">\n"))


(defun vineet/cider-load-open-buffers ()
  "Load all open clojure buffers in the project.
Requires projectile to limit the clojure buffers in the current
project."
  (interactive)
  (dolist (buf (projectile-project-buffers))
    (let ((buf-file-path (buffer-file-name buf)))
      (when (and buf-file-path
                 (string= (file-name-extension buf-file-path) "clj")
                 (not (string= (file-name-nondirectory buf-file-path)
                               "project.clj")))
        (cider-load-buffer buf)))))


(eval-after-load 'cider-mode
  '(progn
     (add-hook 'cider-mode-hook 'eldoc-mode)
     (define-key cider-mode-map (kbd "C-c z") 'cider-selector)
     (setq cider-repl-history-file (concat tempfiles-dirname "nrepl-history.txt")
           cider-repl-history-size most-positive-fixnum
           cider-repl-wrap-history t
           cider-repl-prompt-function 'cider-repl-prompt-on-newline
           cider-repl-display-help-banner nil
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
             ("lookup-on-grimoire" . cider-grimoire-lookup)))))

(eval-after-load 'cider-repl
  '(progn
     (add-hook 'cider-repl-mode-hook 'subword-mode)
     (define-key cider-repl-mode-map (kbd "C-M-q") 'prog-indent-sexp)
     (define-key cider-repl-mode-map (kbd "C-c M-o") 'cider-repl-clear-buffer)))

;;; Integration with REBL
;; Similar to C-x C-e, but sends to REBL
(defun rebl-eval-last-sexp ()
  (interactive)
  (let* ((bounds (cider-last-sexp 'bounds))
         (s (cider-last-sexp))
         (reblized (concat "(cognitect.rebl/inspect " s ")")))
    (cider-interactive-eval reblized nil bounds (cider--nrepl-print-request-map))))

;; Similar to C-M-x, but sends to REBL
(defun rebl-eval-defun-at-point ()
  (interactive)
  (let* ((bounds (cider-defun-at-point 'bounds))
         (s (cider-defun-at-point))
         (reblized (concat "(cognitect.rebl/inspect " s ")")))
    (cider-interactive-eval reblized nil bounds (cider--nrepl-print-request-map))))

;; C-S-x send defun to rebl
;; C-x C-r send last sexp to rebl (Normally bound to "find-file-read-only"... Who actually uses that though?)
(add-hook 'cider-mode-hook
          (lambda ()
            (local-set-key (kbd "C-S-x") #'rebl-eval-defun-at-point)
            (local-set-key (kbd "C-x C-r") #'rebl-eval-last-sexp)))

(provide 'init-cider)
;;; init-cider ends here
