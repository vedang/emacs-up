;;; org-crate-config.el --- Configuration for org-mode
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

(setq org-id-track-globally t
      org-id-locations-file (concat tempfiles-dirname ".org-id-locations")
      org-clock-persist-file (concat tempfiles-dirname "org-clock-save")
      org-meetings-file (concat org-work-directory "/meetings.org"))

(defvar vm/org-updates-heading-id "6a134484-7349-49b7-b580-3045bc87358f")

;;; My personal capture templates
(push '("j" "Journal" entry
        (file+olp+datetree org-journal-file)
        "* %?\n%i" :time-prompt t)
      org-capture-templates)

(push '("h" "Habit" entry
        (file org-default-notes-file)
        "*  %? :habit:  \nSCHEDULED: %t .+1d/3d\n:PROPERTIES:\n:STYLE: habit\n:REPEAT_TO_STATE: TODO\n:END:\n%U\n%a\n  %i")
      org-capture-templates)

(push '("m" "Meeting" entry
        (file+olp+datetree org-meetings-file "Day to Day Meetings")
        "* MEETING %^{To Discuss} %^G \n%U\n- Attendees: %^{Attendees}, Vedang\n- %?"
        :prepend t
        :clock-in t
        :clock-resume t)
      org-capture-templates)

(push '("w" "Work Todo"
        entry (file org-default-notes-file)
        "* TODO %^{Description} [[%^{Type|auto|hsbug|jira}:%^{Ticket number}]] \n%U\n%a\n %i%?")
      org-capture-templates)


;;; My personal tag hotkeys
(push '("engineering_management" . ?e) org-tag-alist)
(push '("refile" . ?r) org-tag-alist)
(push '("bug" . ?b) org-tag-alist)
(push '("study" . ?s) org-tag-alist)
(push '("goal" . ?g) org-tag-alist)
(push '("tweak" . ?t) org-tag-alist)
(push '("productive" . ?p) org-tag-alist)
(push '("feedback" . ?f) org-tag-alist)
(push '("future" . ?F) org-tag-alist)
(push '("writing" . ?W) org-tag-alist)

(setq org-html-head-extra
      "<style type=\"text/css\">  body { font-family:sans-serif; font-size: small; } code {font-size: medium;} </style>"
      org-latex-listings t)

;; https://lists.gnu.org/archive/html/emacs-orgmode/2011-06/msg00716.html
(defun vm/org-extract-link ()
  "Extract the link location at point and put it on the killring."
  (interactive)
  (when (org-in-regexp org-bracket-link-regexp 1)
    (kill-new (org-link-unescape (org-match-string-no-properties 1)))))

(eval-after-load 'org
  '(progn (global-set-key (kbd "C-c M-l") 'vm/org-extract-link)))

(eval-after-load 'org-super-agenda
  '(setq org-super-agenda-groups
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

(setq org-agenda-dim-blocked-tasks nil)

;;; Use `terminal-notifier' to push notifications on osx, if this
;;; program is not installed, ignore notifications.
(defun vm/org-notify-message (msg)
  "Push MSG as a notification via `terminal-notifier'"
  (when (executable-find "terminal-notifier")
    (start-process "page-me"
                   "*debug*"
                   "terminal-notifier"
                   "-activate" "org.gnu.Emacs"
                   "-message" msg
                   "-title" "Org Mode")))

(setq org-show-notification-handler 'vm/org-notify-message)

;;; Use Plantuml for diagrams
;; This value is set in my personal.el file
;; (setq org-plantuml-jar-path "")
(provide 'org-crate-config)
;;; org-crate-config.el ends here.
