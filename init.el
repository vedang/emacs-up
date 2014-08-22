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
(defvar on-my-machine (string-match "ninjakombda" (system-name)))

;;; No GUI
(dolist (mode '(menu-bar-mode tool-bar-mode scroll-bar-mode))
  (when (fboundp mode) (funcall mode -1)))


;;; Some global defs
(setq dotfiles-dirname (file-name-directory (or load-file-name
                                                (buffer-file-name)))
      autoload-file (concat dotfiles-dirname "loaddefs.el")
      custom-file (concat dotfiles-dirname "custom.el")
      personal-file (concat dotfiles-dirname "personal.el")
      el-get-config-file (concat dotfiles-dirname "init-el-get.el")
      features-file (concat dotfiles-dirname "features.el")
      tempfiles-dirname (concat dotfiles-dirname "temp-files/")
      site-lisp-dirname (concat dotfiles-dirname "site-lisp/")
      enhance-dirname (concat dotfiles-dirname "enhance/"))

;; Create temp directories if necessary
(make-directory tempfiles-dirname t)

(load custom-file 'noerror)
(load personal-file 'noerror)
(add-to-list 'load-path site-lisp-dirname)
(add-to-list 'load-path enhance-dirname)
(when (eq system-type 'darwin)
  (require 'osx))

;;; El-Get for great good
(load features-file)
(load el-get-config-file)

;;; Define my programming modes.
(defvar vedang/programming-major-modes
  '(js2-mode c-mode c++-mode conf-mode clojure-mode erlang-mode
             emacs-lisp-mode lisp-mode scheme-mode python-mode)
  "List of programming modes that I use")


;; The order of loading is important. Often times, the next package
;; presumes that the previous one has been loaded.
(require 'core)
(require 'site-lisp)
(require 'utility-functions)

(require 'extra-hooks)
(require 'extra-bindings)
(require 'registers)
(when on-my-machine
  (autoload 'notmuch "notmuch" "notmuch mail" t)
  (eval-after-load 'notmuch
    '(progn (require 'init-notmuch))))

(load-theme 'zenburn t)

(server-start)
(message "My .emacs loaded in %ds"
         (cl-destructuring-bind (hi lo ms psec) (current-time)
           (- (+ hi lo)
              (+ (first *emacs-load-start*)
                 (second *emacs-load-start*)))))
(uf/totd)

;;; init.el ends here.
