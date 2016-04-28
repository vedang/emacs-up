;;; init-paredit.el --- Paredit goodness everywhere
;;; Author: Vedang Manerikar
;;; Created on: 01 Apr 2014
;;; Copyright (c) 2013, 2014 Vedang Manerikar <vedang.manerikar@gmail.com>

;; This file is not part of GNU Emacs.

;;; License:

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the Do What The Fuck You Want to
;; Public License, Version 2, which is included with this distribution.
;; See the file LICENSE.txt

;;; Code:


(defvar paredit-major-modes
  '(emacs-lisp-mode lisp-mode lisp-interaction-mode clojure-mode cider-repl-mode
                    scheme-mode ielm-mode es-mode)
  "List of modes where I want paredit to always work.")

(defun turn-on-paredit ()
  "Utility function to turn on Paredit."
  (enable-paredit-mode)
  (show-paren-mode))

(dolist (m paredit-major-modes)
  (add-hook `,(intern (concat (symbol-name m) "-hook")) 'turn-on-paredit))

(eval-after-load 'paredit
  '(progn
     (define-key paredit-mode-map (kbd "C-o") 'paredit-open-round)
     ;; Unbind `M-s' because it's bound to some handy occur related
     ;; functions by default
     (define-key paredit-mode-map (kbd "M-s") nil)
     (define-key paredit-mode-map (kbd "M-D") 'paredit-splice-sexp)))


(provide 'init-paredit)
;;; init-paredit.el ends here
