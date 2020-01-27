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
(setq org-id-locations-file (concat tempfiles-dirname ".org-id-locations")
      org-clock-persist-file (concat tempfiles-dirname "org-clock-save"))

(defvar vm/org-updates-heading-id "6a134484-7349-49b7-b580-3045bc87358f")

(push '("j" "Journal" entry
        (file+olp+datetree org-journal-file)
        "* %?\n%i" :time-prompt t)
      org-capture-templates)

(push '("h" "Habit" entry
        (file org-default-notes-file)
        "*  %?  \nSCHEDULED: %t .+1d/3d\n:PROPERTIES:\n:STYLE: habit\n:REPEAT_TO_STATE: TODO\n:END:\n%U\n%a\n  %i")
      org-capture-templates)

(push '("u" "Update" checkitem
        ;; vm/org-updates-heading-id
        (id "6a134484-7349-49b7-b580-3045bc87358f")
        "- [ ] %? \nCAPTURED: %U")
      org-capture-templates)


(setq org-html-head-extra
      "<style type=\"text/css\">  body { font-family:sans-serif; font-size: small; } code {font-size: medium;} </style>")

;; https://lists.gnu.org/archive/html/emacs-orgmode/2011-06/msg00716.html
(defun vm/org-extract-link ()
  "Extract the link location at point and put it on the killring."
  (interactive)
  (when (org-in-regexp org-bracket-link-regexp 1)
    (kill-new (org-link-unescape (org-match-string-no-properties 1)))))

(eval-after-load 'org
  '(progn (global-set-key (kbd "C-c M-l") 'vm/org-extract-link)))

(provide 'org-crate-config)
