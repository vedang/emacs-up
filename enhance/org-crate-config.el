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

;; ## Project-specific directories
(defvar vm-base-dir)
(defvar vm-publishing-dir)

(setq vm-base-dir (expand-file-name "~/Tresors/Documents/diary/notes/published")
      vm-publishing-dir (expand-file-name "~/src/prototypes/vedang.me/v7/components/content/resources/content"))

;; ## Convert the Front-Matter from org to md format.
;; Fields I want to support in export, which are not already supported by ox-md:
;;
;; 1. :aliases

(defvar vm/date-time-regexp
  (concat "\\`[[:digit:]]\\{4\\}-[[:digit:]]\\{2\\}-[[:digit:]]\\{2\\}"
          "\\(?:T[[:digit:]]\\{2\\}:[[:digit:]]\\{2\\}:[[:digit:]]\\{2\\}"
          "\\(?:Z\\|[+-][[:digit:]]\\{2\\}:[[:digit:]]\\{2\\}\\)*\\)*\\'")
  "Regexp to match the time stamp strings.

Reference: https://tools.ietf.org/html/rfc3339#section-5.8

Examples:
  2017-07-31
  2017-07-31T17:05:38
  2017-07-31T17:05:38Z
  2017-07-31T17:05:38+04:00
  2017-07-31T17:05:38-04:00.")

(defun vm/yaml-quote-string (val)
  "Wrap VAL with quotes as appropriate.

VAL can be a string, symbol, number or nil.

VAL is returned as-it-is under the following cases:
- It is a number.
- It is a string and is already wrapped with double quotes.
- It is a string and it's value is \"true\" or \"false\".
- It is a string representing a date.
- It is a string representing an integer or float.

If VAL is nil or an empty string, a quoted empty string \"\" is
returned."
  (cond
   ((null val) val)
   ((numberp val) val)
   ((symbolp val) (format "\"%s\"" (symbol-name val)))
   ;; If `val' is a non-empty string
   ((org-string-nw-p val)
    (if (or (and (string= (substring val 0 1) "\"") ;First char is literally a "
                 (string= (substring val -1) "\"")) ;Last char is literally a "
            (string= "true" val)
            (string= "false" val)
            ;; or if it is a date (date, publishDate, expiryDate, lastmod)
            (string-match-p vm/date-time-regexp val))
        val
      ;; Escape the backslashes
      (setq val (replace-regexp-in-string "\\\\" "\\\\\\\\" val))
      ;; Escape the double-quotes
      (setq val (replace-regexp-in-string "\"" "\\\\\""  val))
      (concat "\"" val "\"")))
   ;; Return empty string if anything else
   (t "\"\"")))

(defun vm/get-yaml-list-string (key list)
  "Return KEY's LIST value as a YAML list, represented as a string.

KEY is a string and LIST is a list where an element can be a
symbol, number or a non-empty string.  Examples:

  \(\"abc\" \"def\")   -> \"[\\\"abc\\\", \\\"def\\\"]\"."
  (concat "["
          (mapconcat #'identity
                     (mapcar (lambda (v)
                               (vm/yaml-quote-string
                                (cond
                                 ((symbolp v) (symbol-name v))
                                 ((numberp v) (number-to-string v))
                                 ((org-string-nw-p v) v)
                                 (t (user-error "Invalid element %S in `%s' value %S" v key list)))))
                             list)
                     ", ")
          "]"))

(defun vm/gen-yaml-front-matter (data)
  "Generate front-matter in YAML format, and return that string.

DATA is an alist of the form \((KEY1 . VAL1) (KEY2 . VAL2) .. \),
where KEY is a symbol and VAL is a string."
  (let ((sep "---\n")
        (sign ":")
        (front-matter ""))
    (dolist (pair data)
      (let ((key (symbol-name (car pair)))
            (value (cdr pair)))
        ;; Skip writing front-matter variables whose value is nil
        (unless (or (null value) (and (stringp value) (string= "" value)))
          ;; In YAML, the value portion needs to be wrapped in double
          ;; quotes. Example:
          ;;     title: "My Post"
          (setq front-matter
                (concat front-matter
                        (format "%s%s %s\n"
                                key
                                sign
                                ;; Tags, categories, aliases:
                                ;; front-matter which are lists.
                                (if (listp value)
                                    (vm/get-yaml-list-string key value)
                                  (vm/yaml-quote-string value))))))))
    (concat sep front-matter sep)))

(defun vm/get-front-matter (info)
  "Return the front-matter string.

INFO is a plist used as a communication channel."
  (let* ((title (org-string-nw-p (car (plist-get info :title))))
         (description (org-string-nw-p (plist-get info :description)))
         (date (org-string-nw-p (org-export-get-date info "%Y-%m-%d")))
         (last-updated-at (format-time-string "%Y-%m-%d" (current-time)))
         (aliases (when (plist-get info :aliases)
                    (org-split-string (org-string-nw-p
                                       (plist-get info :aliases))
                                      " ")))
         ;; See: [ref: do_not_use_`org-export-get-category']
         (category (org-element-map (plist-get info :parse-tree) 'keyword
	                 (lambda (kwd)
	                   (when (equal (org-element-property :key kwd) "CATEGORY")
	                     (org-element-property :value kwd)))
	                 info 'first-match))
         (data `((title . ,title)
                 (description . ,description)
                 (date . ,date)
                 (last_updated_at . ,last-updated-at)
                 (aliases . ,aliases)
                 (tags . ,org-file-tags)
                 (category . ,category))))
    (vm/gen-yaml-front-matter data)))

(defun vm/denote-publish-to-md (plist filename pub-dir)
  "Just like `org-md-publish-to-md' but with front-matter.

FILENAME is the filename of the Org file to be published.  PLIST
is the property list for the given project.  PUB-DIR is the
publishing directory.

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

;; # Notes
;; ## Do not use `org-export-get-category'
;; _[tag: do_not_use_`org-export-get-category']_
;;
;; We do not want the fallback behaviour of `org-export-get-category',
;; which is to return the file-name of the file as the category. For
;; us, this field only makes sense when it has been explicitly
;; defined.
(provide 'org-crate-config)
;;; org-crate-config.el ends here.
