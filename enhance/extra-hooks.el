;;; extra-hooks.el --- functions that hook into stuff for the greater good.
;;; Author: Vedang Manerikar
;;; Created on: 13 Oct 2013
;;; Copyright (c) 2013 -- 2021 Vedang Manerikar <vedang.manerikar@gmail.com>
;;; Commentary:
;; No commentary at this point in time.

;; This file is not part of GNU Emacs.

;;; License:

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the Do What The Fuck You Want to
;; Public License, Version 2, which is included with this distribution.
;; See the file LICENSE.txt

;;; Code:


(defun eh/prog-mode-settings ()
  "special settings for programming modes."
  (when (memq major-mode vedang/programming-major-modes)
    (flyspell-prog-mode)         ;; Flyspell mode for comments and strings
    (when (not (equal major-mode 'objc-mode))
      (uf/turn-on-whitespace-mode)) ;; tell me if lines exceed 80 columns
    (uf/pretty-lambdas)))
(add-hook 'find-file-hook 'eh/prog-mode-settings)


;; Indentation hook for C/C++ mode
;; As defined in Documentation/CodingStyle
(defun eh/linux-c-indent ()
  "adjusted defaults for C/C++ mode use with the Linux kernel."
  (interactive)
  (setq tab-width 8)
  (setq indent-tabs-mode nil) ;; force spaces, to work with dumber editors
  (setq c-basic-offset 8))

(defun eh/knr () (c-set-style "K&R"))

(add-hook 'c-mode-hook 'eh/linux-c-indent)
(add-hook 'c-mode-hook 'eh/knr)
(add-hook 'c++-mode-hook 'eh/linux-c-indent)


(defun eh/server-edit ()
  "Close emacsclient buffers using C-x k"
  (when (current-local-map)
    (use-local-map (copy-keymap (current-local-map))))
  (when server-buffer-clients
    (local-set-key (kbd "C-x k") 'server-edit)))
(add-hook 'server-switch-hook 'eh/server-edit)


(provide 'extra-hooks)
