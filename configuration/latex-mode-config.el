;;; latex-mode-config.el --- Settings for LaTeX mode
;;; Author: Vedang Manerikar
;;; Created on: 08 Jan 2012
;;; Time-stamp: "2012-01-08 19:04:08 vedang"
;;; Copyright (c) 2012 Vedang Manerikar <vedang.manerikar@gmail.com>

;; This file is not part of GNU Emacs.

;;; License:

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the Do What The Fuck You Want to
;; Public License, Version 2, which is included with this distribution.
;; See the file LICENSE.txt

;;; Code:


(defun latex-mode-settings ()
  "my settings for LaTeX mode"
  ;always have reftex along with auctex
  (turn-on-reftex)
  (flyspell-mode))

(add-hook 'LaTeX-mode-hook 'latex-mode-settings)

(setq reftex-plug-into-AUCTeX t)
(setq TeX-auto-save t
      TeX-save-query nil
      TeX-parse-self t)

(provide 'latex-mode-config)
