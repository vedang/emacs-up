;;; org-crate-config.el --- Configuration for org-mode
;;; Author: Vedang Manerikar
;;; Created on: 18 Dec 2012
;;; Time-stamp: "2013-09-05 11:14:27 vedang"
;;; Copyright (c) 2012 Vedang Manerikar <vedang.manerikar@gmail.com>

;; This file is not part of GNU Emacs.

;;; License:

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the Do What The Fuck You Want to
;; Public License, Version 2, which is included with this distribution.
;; See the file LICENSE.txt

;;; Code:


(defvar bh/organization-task-id "a87c0695-ab23-44ab-a270-df6e86ec015e")


(setq org-work-directory (concat org-directory "/work")
      org-personal-directory (concat org-directory "/personal")
      org-agenda-files (list org-directory
                             org-work-directory
                             org-personal-directory)
      org-clock-persist-file (concat tempfiles-dirname "org-clock-save"))


(add-to-list 'org-capture-templates
             '("h" "Habit" entry
               (file (concat org-directory "/remember-notes.org"))
               "*  %?   :refile:\n%U\n%a\nSCHEDULED: %t .+1d/3d\n:PROPERTIES:\n:STYLE: habit\n:REPEAT_TO_STATE: NEXT\n:END:\n  %i"))


(setq org-agenda-custom-commands
      (quote (("a" "Work Agenda"
               ((agenda "" ((org-agenda-files (list org-work-directory))
                            (org-agenda-overriding-header
                             "Deadlines and Scheduled")))
                (tags-todo "+release-future|+next+@office-future|+imp+@office-future"
                           ((org-agenda-overriding-header
                             "Do These Tasks Next")
                            (org-agenda-todo-ignore-scheduled t)
                            (org-agenda-todo-ignore-deadlines t)
                            (org-tags-match-list-sublevels t)
                            (org-agenda-sorting-strategy
                             '(effort-up category-keep))))
                (tags-todo "productive+@office|future+@office|fun+@office"
                           ((org-agenda-overriding-header
                             "Other Fun Tasks")
                            (org-agenda-todo-ignore-scheduled t)
                            (org-agenda-todo-ignore-deadlines t)
                            (org-tags-match-list-sublevels t)
                            (org-agenda-sorting-strategy
                             '(effort-up category-keep))))
                (tags "refile"
                      ((org-agenda-overriding-header
                        "Notes and Tasks to Refile")))
                nil))
              ("b" "Personal Agenda"
               ((agenda "" ((org-agenda-files (list org-personal-directory
                                                    (concat org-directory
                                                            "/Birthdays.org")))
                            (org-agenda-overriding-header
                             "Deadlines and Scheduled")))
                (tags-todo "@reading|@study"
                           ((org-agenda-files (list org-personal-directory))
                            (org-agenda-overriding-header
                             "Reading and Study")
                            (org-agenda-todo-ignore-scheduled t)
                            (org-agenda-todo-ignore-deadlines t)
                            (org-tags-match-list-sublevels t)
                            (org-agenda-sorting-strategy
                             '(effort-up priority-down category-keep))))
                (tags-todo "goal|write|study|tweak|+fun-@office"
                           ((org-agenda-files (list org-personal-directory))
                            (org-agenda-overriding-header
                             "Fun Tasks & Tweaks")
                            (org-agenda-todo-ignore-scheduled t)
                            (org-agenda-todo-ignore-deadlines t)
                            (org-tags-match-list-sublevels t)
                            (org-agenda-sorting-strategy
                             '(effort-up priority-down category-keep))))
                (tags-todo "+next-release-someday|+future-someday-@office"
                           ((org-agenda-files (list org-personal-directory))
                            (org-agenda-overriding-header
                             "Next Tasks")
                            (org-agenda-todo-ignore-scheduled t)
                            (org-agenda-todo-ignore-deadlines t)
                            (org-tags-match-list-sublevels t)
                            (org-agenda-sorting-strategy
                             '(effort-up category-keep))))
                (tags-todo "+@errand|+personal"
                           ((org-agenda-overriding-header
                             "Errands & the small stuff")
                            (org-agenda-todo-ignore-scheduled t)
                            (org-agenda-todo-ignore-deadlines t)
                            (org-tags-match-list-sublevels t)
                            (org-agenda-sorting-strategy
                             '(effort-up category-keep))))
                (tags "refile"
                      ((org-agenda-overriding-header
                        "Notes and Tasks to Refile")))
                nil))
              ("h" "Habits" tags-todo "STYLE=\"habit\""
               ((org-agenda-overriding-header "Habits")
                (org-agenda-sorting-strategy
                 '(todo-state-down effort-up category-keep))))
              ("c" "Select default clocking task" tags "LEVEL=1-refile"
               ((org-agenda-skip-function
                 '(org-agenda-skip-subtree-if 'notregexp "^\\* Organization"))
                (org-agenda-overriding-header
                 "Set default clocking task with C-u C-u I")))
              ("d" "Delegated Tasks" todo "DELEGATED"
               ((org-use-tag-inheritance nil)
                (org-agenda-todo-ignore-with-date nil)))
              ("I" "Inheritable Deadlines" todo "TODO|WAITING|IN-REVIEW|IN-QA|WORKING|SOMEDAY"
               ((org-agenda-overriding-header "Inheritable DEADLINEs")
                (org-agenda-skip-function 'fc/skip-non-inheritable-deadlines))))))


(provide 'org-crate-config)
