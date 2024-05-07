;;; org-crate-config.el --- Configuration for org-mode -*- lexical-binding: t -*-
;;; Author: Vedang Manerikar
;;; Created on: 18 Dec 2012
;;; Copyright (c) 2012, 2013 Vedang Manerikar <vedang.manerikar@gmail.com>

;; This file is not part of GNU Emacs.

;;; License:

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the Do What The Fuck You Want to
;; Public License, Version 2, which is included with this distribution.
;; See the file LICENSE.txt

;;; Commentary:

;; Expects `org-directory', `org-work-directory',
;; `org-personal-directory' and `org-agenda-files' to be defined
;; before this file is loaded. (for example, in `personal.el' file)
;; Personal Config:
;; Expects `org-journal-file' to be defined (for example, in `personal.el' file)
;;; Code:

(require 'org)
(require 'org-id)
(require 'org-clock)
(require 'org-agenda)
(setq org-default-notes-file (expand-file-name "brain/daily.org" org-directory)
      org-id-track-globally t
      ;; Don't dim anything in the Agenda. If I want dimming, I will
      ;; ask for it explicitly with the # key-binding in the Agenda.
      org-agenda-dim-blocked-tasks nil)

(defvar vm/org-updates-heading-id "6a134484-7349-49b7-b580-3045bc87358f")

;;; My personal tag hotkeys
(push '("engineering_management" . ?e) org-tag-alist)
(push '("refile" . ?r) org-tag-alist)
(push '("bug" . ?b) org-tag-alist)
(push '("goal" . ?g) org-tag-alist)
(push '("tweak" . ?t) org-tag-alist)
(push '("productive" . ?p) org-tag-alist)
(push '("feedback" . ?f) org-tag-alist)
(push '("future" . ?F) org-tag-alist)
(push '("writing" . ?W) org-tag-alist)


(with-eval-after-load 'org-super-agenda
  (defvar org-super-agenda-groups)
  (setq org-super-agenda-groups
        '((:name "These are your IMPORTANT Tasks"
                 :tag "important"
                 :order 0)
          (:name "Your Meetings today"
                 :and (:date today :not (:habit t :deadline t :scheduled t))
                 :order 1)
          (:name "These are your URGENT Tasks"
                 :not (:habit t :deadline future :scheduled future)
                 :order 2)
          (:name "Habits"
                 :habit t
                 :order 3)
          (:name "Upcoming Tasks"
                 :scheduled t
                 :deadline t
                 :order 4)
          (:name "Clocked today"
                 :log t
                 :order 5)
          ;; After the last group, the agenda will display items that didn't
          ;; match any of these groups, with the default order position of 99
          )))

;;; Use `terminal-notifier' to push notifications on osx, if this
;;; program is not installed, ignore notifications.
(defun vm/org-notify-message (msg)
  "Push MSG as a notification via `terminal-notifier'."
  (when (executable-find "terminal-notifier")
    (start-process "page-me"
                   "*debug*"
                   "terminal-notifier"
                   "-activate" "org.gnu.Emacs"
                   "-message" msg
                   "-title" "Org Mode")))

;;; Settings for org-capture
(require 'org-protocol)
(require 'org-capture)
(defvar org-mode-crate-dir
  (concat user-emacs-directory "el-get/org-mode-crate"))

;;; Add my personal targets for org-capture

;; A capture template to create a task for improving Emacs.
(push `("te" "Improve Emacs Immediate Finish" entry
        (id "B751DE04-D5BD-4CA3-B6F7-7C3943CF8F76")
        (file ,(expand-file-name "capture-templates/todo.org" org-mode-crate-dir))
        :clock-in t
        :clock-resume t
        :immediate-finish t)
      org-capture-templates)

(setq org-show-notification-handler 'vm/org-notify-message)

;;; Use Plantuml for diagrams
;; This value is set in my personal.el file
;; (setq org-plantuml-jar-path "")

(provide 'org-crate-config)
;;; org-crate-config.el ends here.
