;;; init-paredit.el --- Paredit goodness everywhere
;;; Commentary:
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
  '(emacs-lisp-mode lisp-mode clojure-mode cider-repl-mode
                    scheme-mode ielm-mode es-mode)
  "List of modes where I want paredit to always work.")

;;; hideshow.el does not have anything to do with paredit, but I want
;;; the minor-mode to be loaded in exactly the same places as paredit.
;;; Hence adding the coniguration here.

(load-library "hideshow")
(require 'hideshow)
(load-library "paren")
(require 'paren)
(require 'paredit)

(defun turn-on-paredit ()
  "Utility function to turn on Paredit."
  (paredit-mode 1)
  (show-paren-mode 1)
  (hs-minor-mode 1))

(setq show-paren-style 'mixed)

(dolist (m paredit-major-modes)
  (add-hook `,(intern (concat (symbol-name m) "-hook")) 'turn-on-paredit))

(with-eval-after-load 'paredit
  (define-key paredit-mode-map (kbd "C-o") 'paredit-open-round)
  ;; Unbind `M-s' because it's bound to some handy occur related
  ;; functions by default
  (define-key paredit-mode-map (kbd "M-s") nil)
  (define-key paredit-mode-map (kbd "M-D") 'paredit-splice-sexp)
  (define-key paredit-mode-map (kbd "C-A-d") 'paredit-forward-down)
  (define-key paredit-mode-map (kbd "C-A-u") 'paredit-backward-up))

(with-eval-after-load 'hideshow
  ;; Unbind `C-h' this should only be used for help.
  (define-key hs-minor-mode-map (kbd "C-c @ C-h") nil)
  (define-key hs-minor-mode-map (kbd "C-c @ @") 'hs-toggle-hiding)
  (define-key hs-minor-mode-map (kbd "C-c @ 2") 'hs-toggle-hiding))

(provide 'init-paredit)
;;; init-paredit.el ends here
