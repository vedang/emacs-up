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
(defvar on-my-machine (or (string-match "meditatingkombda" (system-name))
                          (string-match "zenkombda" (system-name))))

(defvar emacs-up--version "v3.1.0"
  "The current version of the Emacs Up Starter Kit.")

(defun emacs-up-version ()
  "Return the current version of the Emacs Up Starter Kit."
  (interactive)
  (message "Emacs Up %s" emacs-up--version))

;;; No GUI
(dolist (mode '(menu-bar-mode tool-bar-mode scroll-bar-mode))
  (when (fboundp mode) (funcall mode -1)))

;;; Native compilation
(if (and (fboundp 'native-comp-available-p)
         (native-comp-available-p))
    (setq comp-deferred-compilation t
          comp-speed 2)
  (message "Native complation is *not* available"))

(when (boundp 'comp-eln-load-path)
  (let ((eln-cache-dir (expand-file-name "cache/eln-cache/"
                                         user-emacs-directory))
        (find-exec (executable-find "find")))
    (setcar comp-eln-load-path eln-cache-dir)
    ;; Quitting emacs while native compilation in progress can leave zero byte
    ;; sized *.eln files behind. Hence delete such files during startup.
    (when find-exec
      (call-process find-exec nil nil nil eln-cache-dir
                    "-name" "*.eln" "-size" "0" "-delete"))))

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

  ;; $ brew install notmuch
  (add-to-list 'load-path "/usr/local/share/emacs/site-lisp/")
  (setq notmuch-init-file (concat enhance-dirname "init-notmuch.el"))
  (autoload 'notmuch "notmuch" "notmuch mail" t)
  (eval-after-load 'notmuch
    '(progn (require 'init-notmuch))))

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
'moe"
  (cond
   ;;; Config for Darkula
   ((equal curr-theme 'idea-darkula)
    (progn
      (load-theme 'idea-darkula t)
      (color-theme-idea-darkula)))
   ;;; Config for Billw
   ((equal curr-theme 'billw)
    (progn
      (color-theme-billw)))
   ;;; config for leuven
   ((equal curr-theme 'leuven)
    (progn
      (require 'leuven-theme)
      (require 'leuven-dark-theme)
      (load-theme 'leuven-dark t)))
   ;;; Config for Poet
   ((equal curr-theme 'poet)
    (progn
      (set-face-attribute 'default nil
                          :family "Iosevka"
                          :height 130)
      (set-face-attribute 'fixed-pitch nil
                          :family "Iosevka")
      (set-face-attribute 'variable-pitch nil
                          :family "Baskerville")
      (require 'poet-dark-theme)
      (load-theme 'poet-dark t)))
   ;;; Config for Moe
   ((equal curr-theme 'moe)
    (progn
      ;; Resize titles (optional).
      ;; Markdown and rst should have 6 elements, org should have 9 elements
      (setq moe-theme-resize-title-markdown
            '(2.0 1.7 1.5 1.3 1.0 1.0))
      (setq moe-theme-resize-title-org
            '(2.2 1.8 1.6 1.4 1.2 1.0 1.0 1.0 1.0))
      (setq moe-theme-resize-title-rst
            '(2.0 1.7 1.5 1.3 1.1 1.0))
      (setq moe-theme-highlight-buffer-id t)
      (require 'moe-theme)
      ;; Pune Lat Long: 18.5N, 73.8E
      (setq calendar-latitude +18)
	  (setq calendar-longitude +73)
      (require 'moe-theme-switcher)
      ;; To load moe without customization:
      ;; (load-theme 'moe-dark t)
      ;; (moe-dark)
      ;; Note: The following lines have
      ;; to be after the theme is loaded
      ;; (via `moe-dark' or `moe-light')
      (powerline-moe-theme)
      ;; (moe-theme-set-color 'orange)
      ;; Available colors: blue, orange, green ,magenta, yellow,
      ;; purple, red, cyan, w/b.
      ))))

(add-hook 'after-init-hook (lambda () (vedang/theme-config 'moe)))

(server-start)
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
