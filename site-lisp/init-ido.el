;;; init-ido.el --- Configuration for ido mode -*- lexical-binding: t -*-
;;; Commentary:
;;; Author: Vedang Manerikar
;;; Created on: 18 Oct 2013
;;; Copyright (c) 2013 Vedang Manerikar <vedang.manerikar@gmail.com>

;; This file is not part of GNU Emacs.

;;; License:

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the Do What The Fuck You Want to
;; Public License, Version 2, which is included with this distribution.
;; See the file LICENSE.txt

;;; Code:


(require 'ido-completing-read+)

(defun ido-config ()
  "Function encapsulating all ido configuration that I'm not running (in favor of helm)."
  (ido-mode 'both)
  (ido-everywhere)
  (ido-ubiquitous-mode 1))

(setq ido-enable-flex-matching t
      ido-create-new-buffer 'always
      ido-use-filename-at-point 'guess
      ido-use-virtual-buffers t)


(add-hook 'ido-make-buffer-list-hook 'ido-summary-buffers-to-end)

;;; My old and excellent IDO completion hack. keeping it around for
;;; reference.
;; (defadvice completing-read
;;   (around ido-steroids activate)
;;   "ido on steroids :D from EmacsWiki"
;;   (if (boundp 'ido-cur-list)
;;       ad-do-it
;;     (setq ad-return-value
;;           (ido-completing-read
;;            prompt
;;            (all-completions "" collection predicate)
;;            nil require-match initial-input hist def))))


(provide 'init-ido)
;;; init-ido.el ends here
