;;; init.el --- Root emacs configuration file.
;;; Author: Vedang Manerikar
;;; Created on: 08 Jan 2012
;;; Copyright (c) 2012 Vedang Manerikar <vedang.manerikar@gmail.com>

;; This file is not part of GNU Emacs.

;;; License:

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the Do What The Fuck You Want to
;; Public License, Version 2, which is included with this distribution.
;; See the file LICENSE.txt

;;; Code:;;;


(defvar *emacs-load-start* (current-time))


;;; No GUI
(dolist (mode '(menu-bar-mode tool-bar-mode scroll-bar-mode))
  (when (fboundp mode) (funcall mode -1)))


;;; Some global defs
(setq dotfiles-dirname (file-name-directory (or load-file-name
                                                (buffer-file-name)))
      autoload-file (concat dotfiles-dirname "loaddefs.el")
      vedang-custom-file (concat dotfiles-dirname "custom.el")
      tempfiles-dirname (concat dotfiles-dirname "temp-files/")
      el-get-dirname (concat dotfiles-dirname "el-get/")
      el-get-user-package-directory (concat el-get-dirname "el-get-init-files/")
      site-lisp-dirname (concat dotfiles-dirname "site-lisp/"))

;; Create temp directories if necessary
(make-directory tempfiles-dirname t)


;;; El-Get for great good
(add-to-list 'load-path (concat el-get-dirname "el-get"))

(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.github.com/dimitri/el-get/master/el-get-install.el")
    (let (el-get-master-branch)
      (goto-char (point-max))
      (eval-print-last-sexp))))

(el-get 'sync)


;;; Define my programming modes.
(defvar vedang/programming-major-modes
  '(js2-mode c-mode c++-mode conf-mode clojure-mode erlang-mode
             emacs-lisp-mode lisp-mode scheme-mode python-mode)
  "List of programming modes that I use")
(defvar vedang/lisp-major-modes
  '(emacs-lisp-mode lisp-mode clojure-mode scheme-mode)
  "List of lispy modes that I use")


(add-to-list 'load-path site-lisp-dirname)
(require 'site-lisp)
(require 'local-lisp)
(require 'utility-functions)


;;; init.el ends here.
