;;; osx.el --- emacs configuration for OS X
;;; Author: Vedang Manerikar
;;; Created on: 12 Jul 2012
;;; Time-stamp: "2013-06-19 13:50:18 vedang"
;;; Copyright (c) 2012 Vedang Manerikar <vedang.manerikar@gmail.com>

;; This file is not part of GNU Emacs.

;;; License:

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the Do What The Fuck You Want to
;; Public License, Version 2, which is included with this distribution.
;; See the file LICENSE.txt

;;; Code:;;;


(setq mac-command-modifier 'meta)
(setq mac-option-modifier 'alt)


;; Make cut and paste work with the OS X clipboard
;; github.com/ghoseb/dotemacs


(defun copy-from-osx ()
  (shell-command-to-string "pbpaste"))


(defun paste-to-osx (text &optional push)
  (let ((process-connection-type nil))
    (let ((proc (start-process "pbcopy" "*Messages*" "pbcopy")))
      (process-send-string proc text)
      (process-send-eof proc))))


(when (eq system-type 'darwin)
  (setq interprogram-cut-function 'paste-to-osx)
  (setq interprogram-paste-function 'copy-from-osx))


;; Work around a bug on OS X where system-name is a fully qualified
;; domain name
(when (eq system-type 'darwin)
  (setq system-name (car (split-string system-name "\\."))))


;;; frames
(setq initial-frame-alist '((top . 0)
                            (left . 0)
                            (width . 155)
                            (height . 45)))
(setq frame-title-format "%b")
(setq icon-title-format  "%b")


;; Binaries
(setq magit-git-executable "/usr/local/bin/git"
      vc-git-program "/usr/local/bin/git"
      ispell-program-name "/usr/local/bin/aspell"
      epg-gpg-program "/usr/local/bin/gpg")


;; source dirs
(setq source-directory "/Library/Caches/Homebrew/emacs--git/src/")
(setq find-function-C-source-directory "/Library/Caches/Homebrew/emacs--git/src/")


;; Setup document viewer to read xcode documentation
(require 'xcode-document-viewer)
(setq xcdoc:document-path "~/Library/Developer/Shared/Documentation/DocSets/com.apple.adc.documentation.AppleiOS5_1.iOSLibrary.docset")
(setq xcdoc:open-w3m-other-buffer t)


;; full screen toggle, will only work if emacs is installed from source
(global-set-key (kbd "<f1>") 'ns-toggle-fullscreen)

;; path, because Mac is a bitch.
(add-to-list 'exec-path "/usr/local/bin")
(add-to-list 'exec-path "~/Documents/bin")


;; python path, because Mac doesn't share it's env variables
(setenv "PYTHONPATH"
        "/usr/bin:/usr/local/bin:/usr/local/lib/python2.7/site-packages")


;; Settings for XCode integration and iOS development

;; open objc-mode for .h files as required
(add-to-list 'magic-mode-alist
             `(,(lambda ()
                  (and (string= (file-name-extension buffer-file-name) "h")
                       (re-search-forward "@\\<interface\\>"
                                          magic-mode-regexp-match-limit t)))
               . objc-mode))


;; modifications for objc-mode
(eval-after-load "cc-mode"
  '(progn
     (require 'anything)
     (require 'anything-config)

     (defvar anything-c-source-objc-headline
       '((name . "Objective-C Headline")
         (headline  "^[-+@]\\|^#pragma mark")))

     (defun objc-headline ()
       (interactive)
       ;; Set to 500 so it is displayed even if all methods are not narrowed down.
       (let ((anything-candidate-number-limit 500))
         (anything-other-buffer '(anything-c-source-objc-headline)
                                "*ObjC Headline*")))

     (define-key objc-mode-map
       (kbd "C-c p")
       'objc-headline)

     (defun objc-in-header-file ()
       (let* ((filename (buffer-file-name))
              (extension (car (last (split-string filename "\\.")))))
         (string= "h" extension)))

     (defun objc-jump-to-extension (extension)
       (let* ((filename (buffer-file-name))
              (file-components (append (butlast (split-string filename
                                                              "\\."))
                                       (list extension))))
         (find-file (mapconcat 'identity file-components "."))))

     ;; Assumes that Header and Source file are in same directory
     (defun objc-jump-between-header-source ()
       (interactive)
       (if (objc-in-header-file)
           (objc-jump-to-extension "m")
         (objc-jump-to-extension "h")))

     (define-key objc-mode-map
       (kbd "C-c t")
       'objc-jump-between-header-source)))


(provide 'osx)
