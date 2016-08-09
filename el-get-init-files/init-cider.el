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


(setq cider-repl-history-file (concat tempfiles-dirname "nrepl-history.txt")
      cider-repl-history-size most-positive-fixnum
      cider-repl-wrap-history t
      cider-repl-prompt-function 'cider-repl-prompt-on-newline
      cider-repl-display-help-banner nil
      nrepl-buffer-name-separator "-"
      nrepl-buffer-name-show-port t
      nrepl-log-messages t
      cider-mode-line nil
      cider-annotate-completion-candidates t
      cider-completion-annotations-include-ns 'always
      cider-show-error-buffer 'always
      cider-prompt-for-symbol nil)


(eval-after-load 'cider-mode
  '(progn
     (add-hook 'cider-mode-hook 'eldoc-mode)
     (define-key cider-mode-map (kbd "C-c z") 'cider-selector)))

(eval-after-load 'cider-repl
  '(progn
     (add-hook 'cider-repl-mode-hook 'subword-mode)
     (define-key cider-repl-mode-map (kbd "C-M-q") 'prog-indent-sexp)
     (define-key cider-repl-mode-map (kbd "C-c M-o") 'cider-repl-clear-buffer)))


(provide 'init-cider)

;;; init-cider ends here
