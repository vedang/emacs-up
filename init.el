;;; init.el --- Root emacs configuration file.
;;; Commentary:
;;; Author: Vedang Manerikar
;;; Created on: 08 Jan 2012
;;; Copyright (c) 2012 Vedang Manerikar <vedang.manerikar@gmail.com>

;; This file is not part of GNU Emacs.

;;; License:

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the Do What The Fuck You Want to
;; Public License, Version 2, which is included with this distribution.
;; See the file LICENSE.txt

;;; Code:

(when (version< emacs-version "25")
  (error "Unsupported Emacs Version! Please upgrade to Emacs 25 or above.  Emacs installation instructions: https://www.gnu.org/software/emacs/download.html"))

(defvar *emacs-load-start* (current-time))
;; ninja
;; master
;; humble
(defvar on-my-machine (string-match "meditatingkombda" (system-name)))

(defvar emacs-up--version "v3.1.0"
  "The current version of the Emacs Up Starter Kit.")

(defun emacs-up-version ()
  "Return the current version of the Emacs Up Starter Kit."
  (interactive)
  (message "Emacs Up %s" emacs-up--version))

;;; No GUI
(dolist (mode '(menu-bar-mode tool-bar-mode scroll-bar-mode))
  (when (fboundp mode) (funcall mode -1)))


;;; Some global defs

;; Set a directory for temporary/state related files.
(defvar dotfiles-dirname
  (file-name-directory (or load-file-name
                           (buffer-file-name)))
  "The directory where this code is running from.
Ideally, this will be ~/.emacs.d.")
(defvar tempfiles-dirname
  (concat dotfiles-dirname "temp-files/")
  "A sub-directory to hold temporary files generated by Emacs.")
(defvar autoload-file
  (concat dotfiles-dirname "loaddefs.el")
  "File to generate and store autoload forms in.")
(defvar personal-file
  (concat dotfiles-dirname "personal.el")
  "File to hold personal configuration - config outside of VCS control.")
(defvar el-get-config-file
  (concat dotfiles-dirname "init-el-get.el")
  "File to load packages via el-get and to load associated configuration.")
(defvar features-file
  (concat dotfiles-dirname "features.el")
  "File containing information about which progamming packages / Emacs features to enable.")
(defvar site-lisp-dirname
  (concat dotfiles-dirname "site-lisp/")
  "Extra configuration for packages that are built into Emacs.")
(defvar enhance-dirname
  (concat dotfiles-dirname "enhance/")
  "Extra bits on Emacs Lisp to enhance to user experience.")

(setq custom-file ; File to hold configuration written by Emacs itself
      (concat dotfiles-dirname "custom.el"))

;; Create temp directories if necessary
(make-directory tempfiles-dirname t)

(load custom-file 'noerror)
(load personal-file 'noerror)
(add-to-list 'load-path site-lisp-dirname)
(add-to-list 'load-path enhance-dirname)
(when (eq system-type 'darwin)
  (require 'osx))

;;; El-Get for great good
(load features-file 'noerror)
(load el-get-config-file)

;;; Define my programming modes.
(defvar vedang/programming-major-modes
  '(js2-mode c-mode c++-mode conf-mode clojure-mode erlang-mode
             emacs-lisp-mode lisp-mode scheme-mode python-mode)
  "List of programming modes that I use.")


;; The order of loading is important. Often times, the next package
;; presumes that the previous one has been loaded.
(require 'core)
(require 'site-lisp)
(require 'utility-functions)

(require 'extra-hooks)
(require 'extra-bindings)
(when on-my-machine
  ;; notmuch Emacs support should be installed alongwith notmuch. On
  ;; OSX, this can be done with:

  ;; $ brew install notmuch --with-emacs
  (add-to-list 'load-path "/usr/local/share/emacs/site-lisp/notmuch")
  (setq notmuch-init-file (concat enhance-dirname "init-notmuch.el"))
  (autoload 'notmuch "notmuch" "notmuch mail" t)
  (eval-after-load 'notmuch
    '(progn (require 'init-notmuch))))

(add-hook 'after-init-hook (lambda ()
                             ;; config for leuven
                             (progn (require 'leuven-theme)
                                    (require 'leuven-dark-theme)
                                    (load-theme 'leuven t))
                             ;; config for poet
                             ;; (progn (set-face-attribute 'default nil :family "Iosevka" :height 130)
                             ;;        (set-face-attribute 'fixed-pitch nil :family "Iosevka")
                             ;;        (set-face-attribute 'variable-pitch nil :family "Baskerville")
                             ;;        (require 'poet-dark-monochrome-theme)
                             ;;        (load-theme 'poet-dark-monochrome t))
                             ))

(server-start)
(message "My .emacs loaded in %ds"
         (cl-destructuring-bind (hi lo ms psec) (current-time)
           (- (+ hi lo)
              (+ (first *emacs-load-start*)
                 (second *emacs-load-start*)))))
(uf/totd)

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)


(provide 'init)
;;; init.el ends here
