;;; init-emacs-eclim.el - Configuration for emacs-eclim
;;; Author: Vedang Manerikar
;;; Created on: 28 Oct 2013
;;; Copyright (c) 2013 Vedang Manerikar <vedang.manerikar@gmail.com>

;; This file is not part of GNU Emacs.

;;; License:

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the Do What The Fuck You Want to
;; Public License, Version 2, which is included with this distribution.
;; See the file LICENSE.txt

;;; Code:


(require 'eclim)
(require 'eclimd)
(global-eclim-mode)

(setq eclim-executable "~/bin/eclipse/eclim"
      eclimd-executable "~/bin/eclipse/eclimd"
      eclimd-default-workspace "~/eclipse_workspace"
      help-at-pt-display-when-idle t
      help-at-pt-timer-delay 0.1)

(help-at-pt-set-timer)

(defun iee/eclim-maven-resolve-dependencies ()
  "Run the mvn goal dependency:resolve."
  (interactive)
  (eclim-maven-run "dependency:resolve"))

(defun iee/eclim-maven-update-classpath ()
  "Run the mvn goal eclipse:eclipse"
  (interactive)
  (eclim-maven-run "eclipse:eclipse"))
(eval-after-load 'auto-complete-mode
  '(progn
     (require 'ac-emacs-eclim-source)
     (ac-emacs-eclim-config)))

(provide 'init-emacs-eclim)
