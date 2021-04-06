;;; init-org-mode-crate.el --- Personal Configuration on top of org-mode-crate
;;; Author: Vedang Manerikar
;;; Created on: 21 Oct 2013
;;; Copyright (c) 2012, 2013 Vedang Manerikar <vedang.manerikar@gmail.com>

;; This file is not part of GNU Emacs.

;;; License:

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the Do What The Fuck You Want to
;; Public License, Version 2, which is included with this distribution.
;; See the file LICENSE.txt

;;; Commentary:

;; Set `org-directory' in personal-config, something that is not under
;; version control and can therefore be different on different
;; machines.

;; Similarly, set `bh/organization-task-id' in personal-config

;; Agenda does not have a key-binding by default in Emacs 24.
;; Provide one as a starting point.

;;; Code:

(global-set-key (kbd "<f12>") 'org-agenda)
(message "Press <f12> to get started with your agenda...")

(eval-after-load "org"
  '(progn (require 'org-mode-crate)
          (when on-my-machine
            (require 'org-crate-config))))

(provide 'init-org-mode-crate)
;;; init-org-mode-crate.el ends here
