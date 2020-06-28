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

(setq org-brain-visualize-default-choices 'all
      org-brain-title-max-length 60
      org-brain-include-file-entries nil
      org-brain-file-entries-use-title nil
      org-brain-show-resources t
      org-brain-show-text t
      org-brain-visualize-use-capture-templates t
      org-brain-scan-directories-recursively nil)

;;; Key Bindings
(global-set-key (kbd "C-c v") 'org-brain-visualize)
(define-key org-mode-map (kbd "C-c b") 'org-brain-prefix-map)

;;; Ensure that all org-mode entries have an ID.
(add-hook 'before-save-hook 'org-brain-ensure-ids-in-buffer)

;;; Configuration to integrate `org-noter' with `org-brain'
(defun org-brain-open-org-noter (entry)
  "Open `org-noter' on the ENTRY. If run interactively, get ENTRY from context."
  (interactive (list (org-brain-entry-at-pt)))
  (org-with-point-at (org-brain-entry-marker entry)
    (org-noter)))

(eval-after-load 'org-noter
  '(progn
     (define-key org-brain-visualize-mode-map (kbd "C-c n")
       'org-brain-open-org-noter)))

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

(provide 'init-org-brain)
;;; init-org-brain.el ends here
