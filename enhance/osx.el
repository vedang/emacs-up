;;; osx.el --- emacs configuration for OS X -*- lexical-binding: t -*-
;;; Author: Vedang Manerikar
;;; Created on: 12 Jul 2012
;;; Copyright (c) 2012, 2013 Vedang Manerikar <vedang.manerikar@gmail.com>

;; This file is not part of GNU Emacs.

;;; License:

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the Do What The Fuck You Want to
;; Public License, Version 2, which is included with this distribution.
;; See the file LICENSE.txt

;;; Code:


(defun copy-from-osx ()
  "Make cut and paste work with the OS X clipboard"
  (shell-command-to-string "pbpaste"))

(defun paste-to-osx (text &optional push)
  "Make cut and paste work with the OS X clipboard"
  (let ((process-connection-type nil))
    (let ((proc (start-process "pbcopy" "*Messages*" "pbcopy")))
      (process-send-string proc text)
      (process-send-eof proc))))


(setq mac-command-modifier 'meta
      mac-option-modifier 'alt
      interprogram-cut-function 'paste-to-osx
      interprogram-paste-function 'copy-from-osx
      ;; Work around a bug on OS X where system-name is a fully
      ;; qualified domain name
      system-name (car (split-string system-name "\\."))
      initial-frame-alist '((top . 0) (left . 0) (width . 155) (height . 45))
      frame-title-format "%b"
      icon-title-format  "%b"
      ;; Binaries
      magit-git-executable (or (executable-find "git") "/usr/local/bin/git")
      vc-git-program (or (executable-find "git") "/usr/local/bin/git")
      ispell-program-name (or (executable-find "aspell") "/usr/local/bin/aspell")
      ispell-local-dictionary "british"
      epg-gpg-program (or (executable-find "gpg") "/usr/local/bin/gpg")
      ;; source dirs
      ;; Note: These are hard-coded to my machine.
      source-directory (expand-file-name "~/src/emacs/src/")
      find-function-C-source-directory (expand-file-name "~/src/emacs/src/"))


;; full screen toggle, will only work if emacs is installed from
;; source, and the source is patched with this patch:
;; https://gist.github.com/scotchi/7209145/
(when (fboundp 'toggle-frame-fullscreen)
  (global-set-key (kbd "<f1>") 'toggle-frame-fullscreen))

;;; Bind `frame.el' commands for easier access
(global-set-key (kbd "A-`") 'other-frame)

(provide 'osx)
