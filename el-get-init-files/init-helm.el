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

;;; Code:


(require 'helm-config)

(setq helm-reuse-last-window-split-state t
      helm-move-to-line-cycle-in-source t
      helm-ff-file-name-history-use-recentf t
      helm-buffers-fuzzy-matching t
      helm-recentf-fuzzy-match t
      helm-mini-default-sources '(helm-source-buffers-list
                                  helm-source-recentf
                                  helm-source-bookmarks
                                  helm-source-buffer-not-found)
      helm-grep-ag-command
      "rg --color=always --colors 'match:style:underline' --colors 'match:bg:black' --colors 'match:fg:white' --smart-case --no-heading --line-number %s %s %s"

      ;; Fancy UI follows, turned off by default
      helm-always-two-windows nil
      ;; `helm-show-action-window-other-window' only takes effect if
      ;; `helm-always-two-windows' is non-nil
      helm-show-action-window-other-window 'left
      ;; Note: `helm-commands-using-frame' is for fancy UI where the
      ;; search bar pops up and out for running searches. Enable this
      ;; if you want to show off fancy UX to someone.

      ;; helm-commands-using-frame '(completion-at-point
      ;;                             helm-apropos
      ;;                             helm-eshell-prompts
      ;;                             helm-imenu
      ;;                             helm-imenu-in-all-buffers)

      ;; Similarly, set these to t
      helm-use-frame-when-more-than-two-windows nil
      helm-use-frame-when-dedicated-window nil
      helm-locate-recursive-dirs-command "fd --hidden --type d .*%s.*$ %s"
      helm-ff-auto-update-initial-value t)

(helm-define-key-with-subkeys global-map (kbd "C-c n") ?n 'helm-cycle-resume)

;; Allow flex completion (fuzzy matching)
;; Disabling this as it pushes down the actual results in it's fuzziness.
;; (add-hook 'helm-mode-hook (lambda () (setq completion-styles '(flex))))

(helm-mode +1)

;;; Add ido-completing-read functions for things that don't have
;;; default values in `helm-completing-read-handlers-alist'.

(push '(describe-function . ido-completing-read)
      helm-completing-read-handlers-alist)
(push '(describe-variable . ido-completing-read)
      helm-completing-read-handlers-alist)
(push '(describe-symbol . ido-completing-read)
      helm-completing-read-handlers-alist)
(push '(debug-on-entry . ido-completing-read)
      helm-completing-read-handlers-alist)
(push '(find-function . ido-completing-read)
      helm-completing-read-handlers-alist)
(push '(disassemble . ido-completing-read)
      helm-completing-read-handlers-alist)
(push '(trace-function . ido-completing-read)
      helm-completing-read-handlers-alist)
(push '(trace-function-foreground . ido-completing-read)
      helm-completing-read-handlers-alist)
(push '(trace-function-background . ido-completing-read)
      helm-completing-read-handlers-alist)

(require 'helm-adaptive)
(setq helm-adaptive-history-file nil)
(helm-adaptive-mode +1)

(require 'helm-utils)
(helm-popup-tip-mode +1)
(setq helm-highlight-matches-around-point-max-lines   '(30 . 30)
      helm-window-show-buffers-function #'helm-window-mosaic-fn)
(add-hook 'find-file-hook #'helm-save-current-pos-to-mark-ring)

(require 'helm-info)
(global-set-key (kbd "C-h r") #'helm-info-emacs)

;; I want to use `helm-mini' and `helm-find-files' as my primary entry
;; point into helm.
(global-set-key (kbd "C-x b") 'helm-mini)
(global-set-key (kbd "C-x C-f") 'helm-find-files)

(global-set-key (kbd "C-x c r") nil) ; unset this because I plan to
                                     ; use it as a prefix key.
(global-set-key (kbd "C-x c r b") 'helm-filtered-bookmarks)
(global-set-key (kbd "C-x c r r") 'helm-regexp)
(global-set-key (kbd "M-y") 'helm-show-kill-ring)
(global-set-key (kbd "C-x c SPC") 'helm-all-mark-rings)
(global-set-key (kbd "C-h SPC") 'helm-all-mark-rings)
(global-set-key (kbd "C-x c r i") 'helm-register)

;; rebind tab to run persistent action. now <tab> and <C-j> will both
;; perform persistent actions
(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action)

(when (executable-find "curl")
  (setq helm-net-prefer-curl t))

(defun helm-do-grep-project-root (&optional with-types)
  "Search in current project with.

With WITH-TYPES, ask for file types to search in."
  (interactive "P")
  (let ((default-directory (project-root (project-current t))))
    (call-interactively 'helm-do-grep-ag)))

(global-set-key (kbd "C-x c g a") 'helm-do-grep-project-root)
(global-set-key (kbd "C-c s") 'helm-do-grep-project-root)

(defun helm-do-grep-ag-with-directory (dir)
  "Do `helm-do-grep-ag' with `default-directory' set to DIR."
  (interactive "DDirectory to search in: ")
  (let ((default-directory dir))
    (call-interactively 'helm-do-grep-ag)))

(global-set-key (kbd "C-x c g s") 'helm-do-grep-ag)
(global-set-key (kbd "C-x c g g") 'helm-do-grep-ag-with-directory)

(provide 'init-helm)
;;; init-helm.el ends here
