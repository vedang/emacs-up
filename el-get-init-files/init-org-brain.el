;;; init-org-brain.el --- Personal Configuration on top of org-brain
;;; Author: Vedang Manerikar
;;; Created on: 28 Jun 2020
;;; Copyright (c) 2020 Vedang Manerikar <vedang@hey.com>

;; This file is not part of GNU Emacs.

;;; License:

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the Do What The Fuck You Want to
;; Public License, Version 2, which is included with this distribution.
;; See the file LICENSE.txt

;;; Commentary:

;;; Code:

(defvar org-brain-visualize-default-choices)
(defvar org-brain-child-linebreak-sexp)
(defvar org-brain-title-max-length)
(defvar org-brain-include-file-entries)
(defvar org-brain-file-entries-use-title)
(defvar org-brain-show-resources)
(defvar org-brain-show-text)
(defvar org-brain-visualize-use-capture-templates)
(defvar org-brain-scan-directories-recursively)
(defvar org-brain-default-file-parent)
(defvar org-brain-backlink)
(defvar org-brain-narrow-to-entry)
(defvar org-brain-quit-after-goto)

(setq org-brain-visualize-default-choices 'all
      org-brain-child-linebreak-sexp '0
      org-brain-title-max-length 60
      org-brain-include-file-entries nil
      org-brain-file-entries-use-title nil
      org-brain-show-resources t
      org-brain-show-text t
      org-brain-visualize-use-capture-templates t
      org-brain-scan-directories-recursively nil
      org-brain-default-file-parent "index"
      org-brain-backlink t
      org-brain-narrow-to-entry t
      org-brain-quit-after-goto t)

;;; Key Bindings
(autoload 'helm-brain "org-brain")
(global-set-key (kbd "C-c v") 'helm-brain)
(define-key org-mode-map (kbd "C-c b") 'org-brain-prefix-map)

;;; Ensure that all org-mode entries have an ID.
(add-hook 'before-save-hook 'org-brain-ensure-ids-in-buffer)

;;; Configuration to integrate `org-noter' with `org-brain'
(defun org-brain-open-org-noter (entry)
  "Open `org-noter' on the ENTRY. If run interactively, get ENTRY from context."
  (interactive (list (org-brain-entry-at-pt)))
  (org-with-point-at (org-brain-entry-marker entry)
    (org-noter)))

(with-eval-after-load 'org-noter
  (define-key org-brain-visualize-mode-map
    (kbd "C-c n") 'org-brain-open-org-noter))

;; Setup org-expiry and define a org-agenda function to compare
;; timestamps.
(require 'org-expiry)

(setq org-expiry-inactive-timestamps t)

(defun org-expiry-created-comp (a b)
  "Compare `org-expiry-created-property-name' properties of A and B."
  (let ((ta (ignore-errors
              (org-time-string-to-seconds
               (org-entry-get (get-text-property 0 'org-marker a)
                              org-expiry-created-property-name))))
        (tb (ignore-errors
              (org-time-string-to-seconds
               (org-entry-get (get-text-property 0 'org-marker b)
                              org-expiry-created-property-name)))))
    (cond ((if ta (and tb (< ta tb)) tb) -1)
          ((if tb (and ta (< tb ta)) ta) +1))))

;; Add CREATED property when adding a new org-brain headline entry
(add-hook 'org-brain-new-entry-hook 'org-expiry-insert-created)

;; Finally add a function which lets us watch the entries chronologically
(defun org-brain-timeline ()
  "List all org-brain headlines in chronological order."
  (interactive)
  (let ((org-agenda-files (org-brain-files))
        (org-agenda-cmp-user-defined #'org-expiry-created-comp)
        (org-agenda-sorting-strategy '(user-defined-down)))
    (org-tags-view nil (format "+%s>\"\"" org-expiry-created-property-name))))

(defface aa2u-face '((t . nil))
  "Face for aa2u box drawing characters"
  :group 'org-brain)

(advice-add #'aa2u-1c :filter-return
            (lambda (str) (propertize str 'face 'aa2u-face)))

(defun aa2u-org-brain-buffer ()
  (let ((inhibit-read-only t))
    (make-local-variable 'face-remapping-alist)
    (add-to-list 'face-remapping-alist
                 '(aa2u-face . org-brain-wires))
    (ignore-errors (aa2u (point-min) (point-max)))))

(with-eval-after-load 'ascii-art-to-unicode
  (add-hook 'org-brain-after-visualize-hook 'aa2u-org-brain-buffer))

(defun org-brain-insert-resource-icon (link)
  "Insert an icon, based on content of org-mode LINK."
  (insert (format "%s "
                  (cond ((string-prefix-p "brain:" link)
                         (all-the-icons-fileicon "brain"))
                        ((string-prefix-p "info:" link)
                         (all-the-icons-octicon "info"))
                        ((string-prefix-p "help:" link)
                         (all-the-icons-material "help"))
                        ((string-prefix-p "http" link)
                         (all-the-icons-icon-for-url link))
                        (t
                         (all-the-icons-icon-for-file link))))))

(with-eval-after-load 'all-the-icons
  (add-hook 'org-brain-after-resource-button-functions
            'org-brain-insert-resource-icon))

(defun org-brain-random-reading ()
  "Find something I've already read, to re-read."
  (interactive)
  (org-brain-visualize-random
   (org-brain-headline-entries-in-file
    (expand-file-name "linklog.org" org-brain-path))))

(define-key org-brain-visualize-mode-map (kbd "R")
  'org-brain-random-reading)

;;; Add a resource to and `org-brain-entry' via `org-cliplink'
(with-eval-after-load 'org-cliplink
  (defun org-brain-cliplink-resource ()
    "Add a URL from the clipboard as an org-brain resource.
Suggest the URL title as a description for resource."
    (interactive)
    (let ((url (org-cliplink-clipboard-content)))
      (org-brain-add-resource
       url
       (org-cliplink-retrieve-title-synchronously url)
       t)))

  (define-key org-brain-visualize-mode-map (kbd "L")
    'org-brain-cliplink-resource))

(setq savehist-additional-variables '(org-brain-headline-cache))

(provide 'init-org-brain)
;;; init-org-brain.el ends here
