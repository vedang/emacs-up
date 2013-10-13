;;; extra-hooks.el - functions that hook into stuff for the greater good.
;;; Author: Vedang Manerikar
;;; Created on: 13 Oct 2013
;;; Copyright (c) 2013 Vedang Manerikar <vedang.manerikar@gmail.com>

;; This file is not part of GNU Emacs.

;;; License:

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the Do What The Fuck You Want to
;; Public License, Version 2, which is included with this distribution.
;; See the file LICENSE.txt

;;; Code:;;;


(defun vedang/prog-mode-settings ()
  "special settings for programming modes."
  (when (memq major-mode vedang/programming-major-modes)
    (flyspell-prog-mode)         ;; Flyspell mode for comments and strings
    (when (not (equal major-mode 'objc-mode))
      (uf/turn-on-whitespace-mode)) ;; tell me if lines exceed 80 columns
    (uf/pretty-lambdas)))
(add-hook 'find-file-hook 'vedang/prog-mode-settings)


;; Indentation hook for C/C++ mode
;; As defined in Documentation/CodingStyle
(defun vedang/linux-c-indent ()
  "adjusted defaults for C/C++ mode use with the Linux kernel."
  (interactive)
  (setq tab-width 8)
  (setq indent-tabs-mode nil) ;; force spaces, to work with dumber editors
  (setq c-basic-offset 8))
(add-hook 'c-mode-hook 'vedang/linux-c-indent)
(add-hook 'c-mode-hook (lambda() (c-set-style "K&R")))
(add-hook 'c++-mode-hook 'vedang/linux-c-indent)


(provide 'extra-hooks)
