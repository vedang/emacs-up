;;; init-helm.el --- Configuration for Smex mode
;;; Commentary:
;;; Author: Vedang Manerikar
;;; Created on: 15 Mar 2016
;;; Copyright (c) 2016 Vedang Manerikar <vedang.manerikar@gmail.com>

;; This file is not part of GNU Emacs.

;;; License:

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the Do What The Fuck You Want to
;; Public License, Version 2, which is included with this distribution.
;; See the file LICENSE.txt

;;; Code:;;;


(require 'helm-config)

;; I don't want helm everywhere, only when I explicitly invoke it with
;; keys I've bound it to. Modifying the completing-read handlers to
;; explicitly mention that I want to use ido everywhere.
(setq helm-completing-read-handlers-alist
      '((describe-function . ido-completing-read)
        (describe-variable . ido-completing-read)
        (describe-symbol . ido-completing-read)
        (debug-on-entry . ido-completing-read)
        (find-function . ido-completing-read)
        (disassemble . ido-completing-read)
        (trace-function . ido-completing-read)
        (trace-function-foreground . ido-completing-read)
        (trace-function-background . ido-completing-read)
        (find-tag . ido-completing-read)
        (ffap-alternate-file . nil)
        (tmm-menubar . nil)
        (find-file . nil)
        (execute-extended-command . nil)))

;;; Explicitly turn off global `helm-mode'
(helm-mode -1)

(global-set-key (kbd "C-x c r") nil) ; unset this because I plan to
                                     ; use it as a prefix key.
(global-set-key (kbd "C-x c r b") 'helm-filtered-bookmarks)
(global-set-key (kbd "C-x c r r") 'helm-regexp)
(global-set-key (kbd "C-x c C-b") 'helm-mini)
(global-set-key (kbd "M-y") 'helm-show-kill-ring)
(global-set-key (kbd "C-x c SPC") 'helm-all-mark-rings)
(global-set-key (kbd "C-h SPC") 'helm-all-mark-rings)
(global-set-key (kbd "C-x c r i") 'helm-register)
;; I want to use <C-x c p> for helm-projectile
(global-set-key (kbd "C-x c P") 'helm-list-emacs-process)

;; rebind tab to run persistent action. now <tab> and <C-j> will both
;; perform persistent actions
(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action)
;; make TAB works in terminal
(define-key helm-map (kbd "C-i") 'helm-execute-persistent-action)
;; list actions using C-z
(define-key helm-map (kbd "C-z")  'helm-select-action)

(when (executable-find "curl")
  (setq helm-net-prefer-curl t))

(setq helm-move-to-line-cycle-in-source t
      helm-ff-file-name-history-use-recentf t
      helm-buffers-fuzzy-matching t
      helm-recentf-fuzzy-match t)

;; This is your old M-x.
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

(defun helm-do-grep-project-root (&optional with-types)
  "Search in current project with. With WITH-TYPES, ask for file
types to search in."
  (interactive "P")
  (let ((default-directory (projectile-project-root)))
    (call-interactively 'helm-do-grep-ag)))

(defun helm-do-grep-ag-with-directory (dir)
  "Do `helm-do-grep-ag' with `default-directory' set to DIR."
  (interactive "DDirectory to search in: ")
  (let ((default-directory dir))
    (call-interactively 'helm-do-grep-ag)))

(global-set-key (kbd "C-x c g a") 'helm-do-grep-project-root)
(global-set-key (kbd "C-x c g s") 'helm-do-ag)
;; Move old behaviour to a new key
(global-set-key (kbd "C-x c g g") 'helm-do-grep-ag-with-directory)

(provide 'init-helm)
