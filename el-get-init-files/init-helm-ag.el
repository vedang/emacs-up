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

(global-set-key (kbd "C-x c M-g a") 'helm-do-ag-project-root)
(global-set-key (kbd "C-x c M-g s") 'helm-do-ag)
;; Move old behaviour to a new key
(global-set-key (kbd "C-x c M-g g") 'helm-do-grep-ag)

(provide 'init-helm-ag)
;;; init-helm-ag.el ends here
