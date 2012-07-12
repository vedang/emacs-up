;;; init.el --- Root emacs configuration file.
;;; Author: Vedang Manerikar
;;; Created on: 08 Jan 2012
;;; Time-stamp: "2012-07-12 11:47:41 vedang"
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
(setq *dotfiles-dir* (file-name-directory
                      (or (buffer-file-name) load-file-name))
      *autoload-file* (concat *dotfiles-dir* "loaddefs.el")
      *plugins-dir* (concat *dotfiles-dir* "plugins/")
      *config-dir* (concat *dotfiles-dir* "configuration/")
      *custom-file* (concat *dotfiles-dir* "custom.el")
      *tempfiles-dir* (concat *dotfiles-dir* "temp-files/"))


(defvar vedang/programming-major-modes
  '(js2-mode c-mode c++-mode conf-mode clojure-mode erlang-mode
             emacs-lisp-mode lisp-mode scheme-mode python-mode)
  "List of programming modes that I use")
(defvar vedang/lisp-major-modes
  '(emacs-lisp-mode lisp-mode clojure-mode scheme-mode)
  "List of lispy modes that I use")


;;; Create temp directories if necessary
(make-directory *tempfiles-dir* t)


;;; From nflath.com
;;; add all subdirs under "~/.emacs.d/" to load-path
(if (fboundp 'normal-top-level-add-subdirs-to-load-path)
    (let* ((my-lisp-dir *dotfiles-dir*)
           (default-directory my-lisp-dir)
           (orig-load-path load-path))
      (setq load-path (cons my-lisp-dir nil))
      (normal-top-level-add-subdirs-to-load-path)
      (nconc load-path orig-load-path)))


;;; Require common stuff
(require 'cl)
(require 'uniquify)
(require 'saveplace)


;;; Require my configuration
(load *custom-file* 'noerror)
(require 'utility-functions)
(require 'customizations)
(require 'mode-config)
(require 'key-bindings)
(require 'registers)
(when (eq system-type 'darwin)
  (require 'osx))
(vedang/regen-autoloads)
(server-start)
(message "My .emacs loaded in %ds" (destructuring-bind (hi lo ms) (current-time)
                                     (- (+ hi lo)
                                        (+ (first *emacs-load-start*)
                                           (second *emacs-load-start*)))))
(totd) ; Display Tip Of The Day.

;;; init.el ends here
