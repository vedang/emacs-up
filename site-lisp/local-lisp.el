;;; local-lisp.el --- Configuration for sundry things installed from el-get
;;; Author: Vedang Manerikar
;;; Created on: 08 Jan 2012
;;; Copyright (c) 2012 Vedang Manerikar <vedang.manerikar@gmail.com>

;; This file is not part of GNU Emacs.

;;; License:

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the Do What The Fuck You Want to
;; Public License, Version 2, which is included with this distribution.
;; See the file LICENSE.txt

;;; Code:;;;


(require 'configure-smartparens)

(global-smart-tab-mode 1)
(global-set-key (kbd "C-x g") 'magit-status)

;; TODO: Uncomment when the time is right.
;; ;; This function comes from revive-mode
;; (add-hook 'kill-emacs-hook 'emacs-save-layout)

(provide 'local-lisp)
