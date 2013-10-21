;;; init-ibuffer.el --- Configuration for ibuffer
;;; Author: Vedang Manerikar
;;; Created on: 16 Jan 2012
;;; Copyright (c) 2013 Vedang Manerikar <vedang.manerikar@gmail.com>

;; This file is not part of GNU Emacs.

;;; License:

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the Do What The Fuck You Want to
;; Public License, Version 2, which is included with this distribution.
;; See the file LICENSE.txt

;;; Code:


(autoload 'ibuffer "ibuffer" "List buffers." t)


(defun ii/turn-on-ibuffer ()
  (interactive)
  (ibuffer)
  (ibuffer-switch-to-saved-filter-groups "default"))

(global-set-key (kbd "C-x C-b") 'ii/turn-on-ibuffer)


(setq ibuffer-saved-filter-groups
      (quote (("default"
               ("Programming"
                (or
                 (mode . c-mode)
                 (mode . c++-mode)
                 (mode . erlang-mode)
                 (mode . perl-mode)
                 (mode . python-mode)
                 (mode . emacs-lisp-mode)
                 (mode . clojure-mode)
                 (mode . go-mode)
                 (mode . objc-mode)
                 (mode . makefile-gmake-mode)))
               ("Org"
                (or
                 (mode . org-mode)
                 (mode . markdown-mode)))
               ("IRC"
                (mode . rcirc-mode))
               ("Magit"
                (derived-mode . magit-mode))
               ("Documents"
                (or
                 (mode . LaTeX-mode)
                 (mode . fundamental-mode)))))))


(provide 'init-ibuffer)
