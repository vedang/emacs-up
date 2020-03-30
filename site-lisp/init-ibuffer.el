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
               ("Shell"
                (mode . shell-mode))
               ("Programming"
                (or
                 (mode . c-mode)
                 (mode . c++-mode)
                 (mode . erlang-mode)
                 (mode . perl-mode)
                 (mode . python-mode)
                 (mode . emacs-lisp-mode)
                 (mode . clojure-mode)
                 (mode . clojurescript-mode)
                 (mode . nrepl-repl-mode)
                 (name . "cider-repl")
                 (mode . go-mode)
                 (mode . objc-mode)
                 (mode . mhtml-mode)
                 (mode . css-mode)
                 (mode . scss-mode)
                 (mode . java-mode)
                 (mode . makefile-gmake-mode)))
               ("Writing"
                (or
                 (mode . org-mode)
                 (derived-mode . org-mode)
                 (mode . org-agenda-mode)
                 (mode . org-brain-visualize-mode)
                 (mode . markdown-mode)
                 (mode . notmuch-message-mode)
                 (mode . LaTeX-mode)
                 (mode . fundamental-mode)
                 (mode . text-mode)))
               ("IRC"
                (mode . rcirc-mode))
               ("Magit"
                (or
                 (derived-mode . magit-mode)
                 (name . "magit")))))))


(provide 'init-ibuffer)
;;; init-ibuffer ends here
