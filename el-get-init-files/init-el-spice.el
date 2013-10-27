;;; init-el-spice.el --- Configuration for El-Spice
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

(add-hook 'emacs-lisp-mode-hook 'el-spice-mode)
(add-hook 'lisp-interaction-mode-hook 'el-spice-mode)

(provide 'init-el-spice)
