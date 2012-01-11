;;; registers.el --- a brilliant idea: pre-defining registers
;;; (from emacs-starter-kit)
;;; Author: Vedang Manerikar
;;; Created on: 08 Jan 2012
;;; Time-stamp: "2012-01-11 12:11:55 vedang"
;;; Copyright (c) 2012 Vedang Manerikar <vedang.manerikar@gmail.com>

;; This file is not part of GNU Emacs.

;;; License:

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the Do What The Fuck You Want to
;; Public License, Version 2, which is included with this distribution.
;; See the file LICENSE.txt

;;; Code:


(dolist (r `((?i (file . ,(concat *dotfiles-dir* "init.el")))
             (?o (file . ,(concat *dotfiles-dir* "configuration/org-mode-config.el")))
             (?m (file . ,(concat *dotfiles-dir* "configuration/mode-config.el")))))
  (set-register (car r) (cadr r)))

(provide 'registers)
