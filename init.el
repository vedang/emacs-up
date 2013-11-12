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
      custom-file (concat dotfiles-dirname "custom.el")
      personal-file (concat dotfiles-dirname "personal.el")
      tempfiles-dirname (concat dotfiles-dirname "temp-files/")
      el-get-dirname (concat dotfiles-dirname "el-get/")
      el-get-user-package-directory (concat dotfiles-dirname
                                            "el-get-init-files/")
      el-get-my-recipes (concat el-get-user-package-directory
                                "personal-recipes/")
      site-lisp-dirname (concat dotfiles-dirname "site-lisp/")
      enhance-dirname (concat dotfiles-dirname "enhance/"))

;; Create temp directories if necessary
(make-directory tempfiles-dirname t)


;;; El-Get for great good
(add-to-list 'load-path (concat el-get-dirname "el-get"))

(defvar el-get-my-packages '(ac-nrepl
                             ace-jump-mode
                             auto-complete
                             cider
                             clojure-mode
                             color-theme-zenburn
                             emacs-eclim
                             el-spice
                             flymake-cursor
                             ibuffer-vc
                             magit
                             markdown-mode
                             multiple-cursors
                             org-mode-crate
                             smart-tab
                             smartparens
                             smex
                             unbound
                             wgrep
                             writegood
                             yasnippet))

(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.github.com/dimitri/el-get/master/el-get-install.el")
    (let (el-get-master-branch
          el-get-install-skip-emacswiki-recipes)
      (goto-char (point-max))
      (eval-print-last-sexp))))

(add-to-list 'el-get-recipe-path el-get-my-recipes)
(el-get 'sync el-get-my-packages)


;;; Define my programming modes.
(defvar vedang/programming-major-modes
  '(js2-mode c-mode c++-mode conf-mode clojure-mode erlang-mode
             emacs-lisp-mode lisp-mode scheme-mode python-mode)
  "List of programming modes that I use")
(defvar vedang/lisp-major-modes
  '(emacs-lisp-mode lisp-mode clojure-mode scheme-mode)
  "List of lispy modes that I use")


;; The order of loading is important. Often times, the next package
;; presumes that the previous one has been loaded.
(add-to-list 'load-path site-lisp-dirname)
(require 'core)
(require 'site-lisp)
(require 'utility-functions)

(add-to-list 'load-path enhance-dirname)
(require 'extra-hooks)
(require 'extra-bindings)
(require 'registers)
(when (eq system-type 'darwin)
  (require 'osx))

(load-theme 'zenburn t)
(load custom-file 'noerror)
(load personal-file 'noerror)

(server-start)
(message "My .emacs loaded in %ds"
         (cl-destructuring-bind (hi lo ms psec) (current-time)
           (- (+ hi lo)
              (+ (first *emacs-load-start*)
                 (second *emacs-load-start*)))))
(uf/totd)

;;; init.el ends here.
