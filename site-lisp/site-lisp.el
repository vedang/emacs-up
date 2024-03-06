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
(setq save-place-file
      (concat tempfiles-dirname "saveplace"))
(save-place-mode 1)
(setq savehist-file
      (concat tempfiles-dirname "savehist"))
(savehist-mode 1)
(setq project-list-file
      (concat tempfiles-dirname "projects"))

(with-eval-after-load 'rcirc
  (require 'init-rcirc))
(with-eval-after-load 'eldoc
  (setq eldoc-minor-mode-string nil
        eldoc-idle-delay 0.75))

(require 'eshell)
(require 'em-smart)
(setq eshell-where-to-jump 'begin)
(setq eshell-review-quick-commands nil)
(setq eshell-smart-space-goes-to-end t)

;;; Configuration for Flymake
(with-eval-after-load 'flymake
  (define-key flymake-mode-map (kbd "C-c # n") 'flymake-goto-next-error)
  (define-key flymake-mode-map (kbd "C-c # p") 'flymake-goto-prev-error)
  (define-key flymake-mode-map (kbd "C-c # l") 'flymake-show-buffer-diagnostics)
  (define-key flymake-mode-map (kbd "C-c # L") 'flymake-show-project-diagnostics)
  (setq flymake-no-changes-timeout 0.75))

;;; Configuration for Eglot
(with-eval-after-load 'eglot
  (define-key eglot-mode-map (kbd "C-c e r") 'eglot-rename)
  (define-key eglot-mode-map (kbd "C-c e o") 'eglot-code-action-organize-imports)
  (define-key eglot-mode-map (kbd "C-c e h") 'eldoc)
  (define-key eglot-mode-map (kbd "C-c e c") 'eglot-code-actions)
  (define-key eglot-mode-map (kbd "C-c e f") 'eglot-format)
  (setq eglot-extend-to-xref t))

;;; Macros for rejister
(defalias 'idbi-txn-date
  (kmacro "M-f M-f C-f C-SPC M-f C-w M-b M-b C-y - M-f C-f C-SPC M-f C-w M-b C-y - M-f C-d C-d C-n C-a C-s / C-b C-b C-b"))

(provide 'site-lisp)
;;; site-lisp.el ends here
