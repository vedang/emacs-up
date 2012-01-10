;;; elpa-config.el --- Configuration for ELPA: the emacs package manager
;;; Author: Vedang Manerikar
;;; Created on: 08 Jan 2012
;;; Time-stamp: "2012-01-10 16:38:51 vedang"
;;; Copyright (c) 2012 Vedang Manerikar <vedang.manerikar@gmail.com>

;; This file is not part of GNU Emacs.

;;; License:

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the Do What The Fuck You Want to
;; Public License, Version 2, which is included with this distribution.
;; See the file LICENSE.txt

;;; Code:


(require 'package)

;;; External repos
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives
             '("ELPA" . "http://tromey.com/elpa/") t)

(package-initialize)

;;; From emacs-starter-kit
(defvar starter-packages '(magit gist auctex paredit color-theme
                                 markdown-mode)
  "Libraries that should be installed by default.")

(when (not package-archive-contents)
  (package-refresh-contents))

(dolist (p starter-packages)
  (when (not (package-installed-p p))
    (package-install p)))


(provide 'elpa-config)
