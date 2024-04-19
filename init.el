;;; init.el --- Root emacs configuration file. -*- lexical-binding: t -*-
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

;;; To debug problems with packages (example org), a great technique
;;; is to drop into the debugger immediately after the problematic
;;; package loads:
;; (with-eval-after-load 'org (debug))

(when (version< emacs-version "25")
  (error "Unsupported Emacs Version! Please upgrade to Emacs 25 or above.  Emacs installation instructions: https://www.gnu.org/software/emacs/download.html"))

(defvar *emacs-load-start* (current-time))
(server-start)
;; ninja
;; master
;; humble
;; meditating
(defvar on-my-mac-machine (string-match "raagitkombdi" (system-name)))
(defvar on-my-linux-machine (string-match "zenkombda" (system-name)))
(defvar on-my-machine (or on-my-mac-machine on-my-linux-machine))

(defvar emacs-up--version "v3.1.0"
  "The current version of the Emacs Up Starter Kit.")

(defun emacs-up-version ()
  "Return the current version of the Emacs Up Starter Kit."
  (interactive)
  (message "Emacs Up %s" emacs-up--version))

;;; Some global defs

;; Set a directory for temporary/state related files.
(defvar dotfiles-dirname
  (file-name-directory (or load-file-name (buffer-file-name)))
  "The directory where this code is running from.
Ideally, this will be ~/.emacs.d.")
(defvar autoload-file
  (concat dotfiles-dirname "loaddefs.el")
  "File to generate and store autoload forms in.")
(defvar personal-file
  (concat dotfiles-dirname "personal.el")
  "File to hold personal configuration - config outside of VCS control.")
(defvar el-get-config-file
  (concat dotfiles-dirname "init-el-get.el")
  "File to load packages via el-get and to load associated configuration.")

(defvar site-lisp-dirname
  (concat dotfiles-dirname "site-lisp/")
  "Extra configuration for packages that are built into Emacs.")
(defvar enhance-dirname
  (concat dotfiles-dirname "enhance/")
  "Extra bits on Emacs Lisp to enhance to user experience.")

(setq custom-file ; File to hold configuration written by Emacs itself
      (concat dotfiles-dirname "custom.el"))

;; Create temp directories if necessary
(make-directory (concat user-emacs-directory "temp-files") t)

(load custom-file nil nil t t)
(load personal-file nil nil t t)
(add-to-list 'load-path site-lisp-dirname)
(add-to-list 'load-path enhance-dirname)

;;; El-Get for great good
(load el-get-config-file nil nil t t)
(when (eq system-type 'darwin)
  (require 'osx))

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

  ;; $ brew install notmuch
  (add-to-list 'load-path "/usr/local/share/emacs/site-lisp/")
  (setq notmuch-init-file (concat enhance-dirname "init-notmuch.el"))
  (autoload 'notmuch "notmuch" "notmuch mail" t)
  (with-eval-after-load 'notmuch
    (require 'init-notmuch)))

;;; NOTE: Personal Experience: Theme stuff needs to load after
;;; everything else has loaded for the least number of surprises.
;;; Hence creating a function to capture this configuration and adding
;;; it as a hook to run post init.
(defun vedang/theme-config (curr-theme)
  "All the configuration for the Emacs Themes I like.

CURR-THEME is the theme that gets loaded. Available values:
'idea-darkula
'billw
'leuven
'poet
'moe
'modus"
  (cond
   ;;; Config for Darkula
   ((equal curr-theme 'idea-darkula)
    (progn
      (load-theme 'idea-darkula t)))
   ;;; Config for Billw
   ((equal curr-theme 'billw)
    (progn
      (color-theme-billw)))
   ;;; config for leuven
   ((equal curr-theme 'leuven)
    (progn
      (load-theme 'leuven-dark t)))
   ;;; Config for Poet
   ((equal curr-theme 'poet)
    (progn
      (load-theme 'poet-dark t)))
   ;;; Config for Moe
   ((equal curr-theme 'moe)
    (progn
      ;; Resize titles (optional).
      ;; Markdown and rst should have 6 elements, org should have 9 elements
      (setq moe-theme-resize-title-markdown
            '(2.0 1.7 1.5 1.3 1.0 1.0))
      ;; (setq moe-theme-resize-title-org
      ;;       '(2.2 1.8 1.6 1.4 1.2 1.0 1.0 1.0 1.0))
      (setq moe-theme-resize-title-org nil)
      (setq moe-theme-resize-title-rst
            '(2.0 1.7 1.5 1.3 1.1 1.0))
      (setq moe-theme-highlight-buffer-id t)
      (require 'moe-theme)
      ;; Pune Lat Long: 18.5N, 73.8E
      (setq calendar-latitude +18)
	  (setq calendar-longitude +73)
      ;; To enable automatic switching between day and night (based on
      ;; `calendar-latitude' and `calendar-longitude'), uncomment:
      ;; (require 'moe-theme-switcher)
      ;; To disable automatic switching once you have enabled it:
      ;; (moe-theme-switcher-disable)
      (moe-dark)
      ;; Note: The following lines have
      ;; to be after the theme is loaded
      ;; (via `moe-dark' or `moe-light')
      (powerline-moe-theme)
      (moe-theme-apply-color 'purple)
      ;; Available colors: blue, orange, green ,magenta, yellow,
      ;; purple, red, cyan, w/b.

      ;; To choose a color randomly:
      ;; (moe-theme-random-color)
      ))

   ((equal curr-theme 'default-dark)
    (progn
      ;;; dark on light default
      (set-background-color "grey15")
      (set-foreground-color "white")
      (spaceline-all-the-icons-theme)))

   ((equal curr-theme 'default-light)
    (progn
      ;;; light on dark default
      (set-background-color "white")
      (set-foreground-color "black")))

   ;; Config for Modus themes
   ((equal curr-theme 'modus)
    (progn
      (require-theme 'modus-themes)
      ;; Add all your customizations prior to loading the themes.
      ;; (setq modus-themes-italic-constructs t
      ;;       modus-themes-bold-constructs nil)
      (load-theme 'modus-vivendi)))))

;;; NOTE: We also have theme configuration in `init-el-get.el', search
;;; for calls to `load-theme' in that file. Those are all turned off
;;; when the line below is turned on.
;; (add-hook 'after-init-hook (lambda () (vedang/theme-config 'modus)))

(message "My .emacs loaded in %ds"
         (cl-destructuring-bind (hi lo ms psec) (current-time)
           (- (+ hi lo)
              (+ (first *emacs-load-start*)
                 (second *emacs-load-start*)))))
;; (uf/totd)

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
;; (package-initialize)

(provide 'init)
;;; init.el ends here
