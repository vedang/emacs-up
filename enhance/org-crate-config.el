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

;; (setq org-ellipsis " ")
(setq org-ellipsis "⤵")

;;; # Configuration for publishing my denote files with `ox-publish'
;; ## Require the libraries that I depend on
(require 'ox-publish)
(require 'ox-hugo)
(setq org-hugo-front-matter-format "yaml")

;; ## Project-specific directories
(defvar vm-base-dir)
(defvar vm-publishing-dir)

(setq vm-base-dir (expand-file-name "~/Tresors/Documents/diary/notes/published")
      vm-publishing-dir (expand-file-name "~/src/prototypes/vedang.me/v7/components/content/resources/content"))

;; ## Convert the Front-Matter from org to md format.
(defun vm/get-front-matter (info)
  "Return the front-matter string.

INFO is a plist used as a communication channel."
  (let* ((title (org-string-nw-p (car (plist-get info :title))))
         (description (org-string-nw-p (plist-get info :description)))
         (date (org-string-nw-p (org-export-get-date info "%Y-%m-%d")))
         (aliases (when (plist-get info :aliases)
                    (org-split-string (org-string-nw-p
                                       (plist-get info :aliases))
                                      " ")))
         (category (org-export-get-category (plist-get info :parse-tree) info))
         (data `((title . ,title)
                 (description . ,description)
                 (date . ,date)
                 (aliases . ,aliases)
                 (tags . ,org-file-tags)
                 (category . ,category))))
    (org-hugo--gen-yaml-front-matter data)))

(defun vm/denote-publish-to-md (plist filename pub-dir)
  "Just like `org-md-publish-to-md' but with front-matter.

FILENAME is the filename of the Org file to be published.  PLIST
is the property list for the given project.  PUB-DIR is the
publishing directory.

This function adds front-matter using `org-hugo--get-front-matter' to
the exported file.

Return output file name."
  (interactive)
  (let* ((ast (org-element-parse-buffer))
         (info (org-combine-plists
                (list :parse-tree ast)
                (org-export--get-export-attributes 'md)
                (org-export--get-buffer-attributes)
                (org-export-get-environment 'md)))
         ;; (_tempinfo (setq tempinfo info)) ;; for debugging
         (fm (vm/get-front-matter info))
         (outfile (org-publish-org-to 'md filename ".md" plist pub-dir)))
    (with-temp-buffer
      (insert fm)
      (insert "\n\n")
      (insert-file-contents outfile)
      (write-file outfile))
    outfile))

(setq org-publish-project-alist
      `(("vedangme" .
         (:base-directory ,vm-base-dir
                          :publishing-directory ,vm-publishing-dir
                          :publishing-function vm/denote-publish-to-md
                          :recursive nil
                          :exclude-tags ("noexport" "draft" "private")
                          :section-numbers nil
                          :with-creator nil
                          :auto-sitemap t
                          :makeindex t))))

;;; Use Plantuml for diagrams
;; This value is set in my personal.el file
;; (setq org-plantuml-jar-path "")
(provide 'org-crate-config)
;;; org-crate-config.el ends here.
