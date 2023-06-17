;;; site-lisp.el --- Change the behavior of things that come built
;;; into Emacs
;;; Commentary:
;;; Author: Vedang Manerikar
;;; Created on: 22 Sep 2013
;;; Copyright (c) 2013 Vedang Manerikar <vedang.manerikar@gmail.com>

;; This file is not part of GNU Emacs.

;;; License:

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the Do What The Fuck You Want to
;; Public License, Version 2, which is included with this distribution.
;; See the file LICENSE.txt

;;; Code:

(require 'cl-lib)

(require 'uniquify)
(setq uniquify-buffer-name-style 'reverse
      uniquify-separator "|"
      uniquify-after-kill-buffer-p t
      uniquify-ignore-buffers-re "^\\*")

(require 'saveplace)

(add-hook 'occur-mode-hook 'next-error-follow-minor-mode)

(require 'dired)
(require 'dired-x)
(require 'wdired)
(setq wdired-allow-to-change-permissions t
      wdired-use-interactive-rename nil
      wdired-confirm-overwrite t)
(define-key dired-mode-map (kbd "e")
  (lambda ()
    (interactive)
    (eww-open-file (dired-get-file-for-visit))))
(add-hook 'dired-mode-hook #'dired-hide-details-mode)
(global-set-key (kbd "C-x D") #'find-dired)

;;; commented out to see if this is causing problems in Emacs 29
;; (require 'dired+)

;; customizations for auto-indentation
(defadvice yank (after indent-region activate)
  (if (member major-mode vedang/programming-major-modes)
      (let ((mark-even-if-inactive t))
        (indent-region (region-beginning) (region-end) nil))))

(defadvice yank-pop (after indent-region activate)
  (if (member major-mode vedang/programming-major-modes)
      (let ((mark-even-if-inactive t))
        (indent-region (region-beginning) (region-end) nil))))


(defun sl/set-auto-major-mode ()
  "When I create a temporary buffer, it should auto-detect the
right mode to start in the buffer."
  (let ((buffer-file-name (or buffer-file-name
                              (buffer-name))))
    (set-auto-mode)))
;(setq default-major-mode 'sl/set-auto-major-mode)


;; Bindings
(global-set-key (kbd "M-j") #'pop-to-mark-command)
(global-set-key (kbd "RET") #'reindent-then-newline-and-indent)
(global-set-key (kbd "A-l") #'goto-line)
(global-set-key (kbd "C-x n r") #'narrow-to-region)
(global-set-key (kbd "C-x \\") #'align-regexp)
(global-set-key (kbd "M-/") #'hippie-expand)
(global-set-key (kbd "C-c y") #'bury-buffer)
(global-set-key (kbd "<f5>") #'revert-buffer)
(global-set-key (kbd "C-x m") #'eshell)
(global-set-key (kbd "C-x M") #'shell)
(global-set-key (kbd "C-c a") #'org-agenda)
(global-set-key (kbd "C-+") #'text-scale-increase)
(global-set-key (kbd "C--") #'text-scale-decrease)
(global-set-key (kbd "C-S-t") #'transpose-sexps)
;; M-q is eaten inside VirtualBox on Mac.
;; Re-mapping some important functions which depend on M-q
(global-set-key (kbd "C-c q") #'fill-paragraph)
(global-set-key (kbd "C-s-q") #'prog-indent-sexp)


;; Aliases for common functions
(defalias 'rvt 'revert-buffer)
(defalias 'dtw 'delete-trailing-whitespace)
(defalias 'yes-or-no-p 'y-or-n-p)


;; Require other site-lisp configuration

(require 'init-ido)
(require 'init-ibuffer)
(require 'init-isearch)
(require 'init-recentf)
(require 'init-flyspell)
(require 'init-sql)
(save-place-mode 1)
(savehist-mode 1)

(eval-after-load 'rcirc
  '(require 'init-rcirc))
(eval-after-load 'eldoc
  '(setq eldoc-minor-mode-string nil))

(require 'eshell)
(require 'em-smart)
(setq eshell-where-to-jump 'begin)
(setq eshell-review-quick-commands nil)
(setq eshell-smart-space-goes-to-end t)

(provide 'site-lisp)
;;; site-lisp.el ends here
