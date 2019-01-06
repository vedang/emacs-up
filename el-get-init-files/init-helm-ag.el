;;; init-helm-ag.el --- Configuration for helm-ag mode
;;; Commentary:
;;; Author: Vedang Manerikar
;;; Created on: 16 Oct 2016
;;; Copyright (c) 2016 Vedang Manerikar <vedang.manerikar@gmail.com>

;; This file is not part of GNU Emacs.

;;; License:

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the Do What The Fuck You Want to
;; Public License, Version 2, which is included with this distribution.
;; See the file LICENSE.txt

;;; Code:

(require 'helm-ag)

(setq helm-ag-insert-at-point 'symbol
      helm-ag-fuzzy-match t)

(defun helm-do-grep-ag-with-directory (dir)
  "Do `helm-do-grep-ag' with `default-directory' set to DIR."
  (interactive "DDirectory: ")
  (let ((default-directory dir))
    (call-interactively 'helm-do-grep-ag)))

(global-set-key (kbd "C-x c g a") 'helm-do-ag-project-root)
(global-set-key (kbd "C-x c g s") 'helm-do-ag)
;; Move old behaviour to a new key
(global-set-key (kbd "C-x c g g") 'helm-do-grep-ag-with-directory)

(provide 'init-helm-ag)
;;; init-helm-ag.el ends here
