;;; init-notmuch.el --- configuration for using notmuch to manage email
;;; Author: Vedang Manerikar
;;; Created on: 07th June 2014
;;; Copyright (c) 2014 Vedang Manerikar <vedang.manerikar@gmail.com>

;; This file is not part of GNU Emacs.

;;; License:

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the Do What The Fuck You Want to
;; Public License, Version 2, which is included with this distribution.
;; See the file LICENSE.txt

;;; Commentary:
;; Put this file somewhere on your load-path, after notmuch is loaded.
;; Eg:
;; (require 'notmuch)
;; (require 'init-notmuch)
;; The variable `notmuch-mail-dir' needs to be defined (for example,
;; in your personal.el file)
;;; Code:

(setq user-mail-address (notmuch-user-primary-email)
      user-full-name (notmuch-user-name)
      message-send-mail-function 'message-send-mail-with-sendmail
      ;; we substitute sendmail with msmtp
      sendmail-program (executable-find "msmtp")
      message-sendmail-envelope-from 'header
      mail-specify-envelope-from t
      notmuch-archive-tags '("-inbox" "-unread" "+archived")
      notmuch-show-mark-read-tags '("-unread")
      notmuch-search-oldest-first nil
      notmuch-show-indent-content nil
      mml-secure-smime-sign-with-sender t
      notmuch-hooks-dir (expand-file-name ".notmuch/hooks" notmuch-mail-dir))

;;; My Notmuch start screen:
(progn
  (setq notmuch-saved-searches nil)
  (push '(:name "Inbox"
                :query "tag:inbox AND tag:screened"
                :key "i"
                :search-type 'tree)
        notmuch-saved-searches)
  (push '(:name "Previously Seen"
                :query "tag:screened AND NOT tag:unread"
                :key "I")
        notmuch-saved-searches)
  (push '(:name "Unscreened"
                :query "tag:inbox AND NOT tag:screened"
                :key "s")
        notmuch-saved-searches)
  (push '(:name "The Feed"
                :query "tag:thefeed"
                :key "f"
                :search-type 'tree)
        notmuch-saved-searches)
  (push '(:name "The Papertrail"
                :query "tag:/ledger/"
                :key "p")
        notmuch-saved-searches))

;; Integrate with org-mode
(require 'ol-notmuch)

(eval-after-load 'notmuch-show
  '(progn
     ;; Bindings in `notmuch-show-mode'
     (define-key notmuch-show-mode-map (kbd "r")
       'notmuch-show-reply)
     (define-key notmuch-show-mode-map (kbd "R")
       'notmuch-show-reply-sender)
     (define-key notmuch-show-mode-map (kbd "C")
       'vedang/notmuch-reply-later)

     ;; The HEY workflow bindings
     (define-key notmuch-show-mode-map (kbd "I")
       'vedang/notmuch-move-sender-to-screened)

     ;; Bindings in `notmuch-search-mode'
     (define-key notmuch-search-mode-map (kbd "r")
       'notmuch-search-reply-to-thread)
     (define-key notmuch-search-mode-map (kbd "R")
       'notmuch-search-reply-to-thread-sender)
     (define-key notmuch-search-mode-map (kbd "/")
       'notmuch-search-filter)
     (define-key notmuch-search-mode-map (kbd "A")
       'vedang/notmuch-archive-all)
     (define-key notmuch-search-mode-map (kbd "D")
       'vedang/notmuch-delete-all)
     (define-key notmuch-search-mode-map (kbd "L")
       'vedang/notmuch-filter-by-from)
     (define-key notmuch-search-mode-map (kbd ";")
       'vedang/notmuch-search-by-from)
     (define-key notmuch-search-mode-map (kbd "d")
       'vedang/notmuch-search-delete-and-archive-thread)

     ;; The HEY Workflow Bindings
     (define-key notmuch-search-mode-map (kbd "S")
       'vedang/notmuch-move-sender-to-spam)
     (define-key notmuch-search-mode-map (kbd "I")
       'vedang/notmuch-move-sender-to-screened)
     (define-key notmuch-search-mode-map (kbd "P")
       'vedang/notmuch-move-sender-to-papertrail)
     (define-key notmuch-search-mode-map (kbd "f")
       'vedang/notmuch-move-sender-to-thefeed)
     (define-key notmuch-search-mode-map (kbd "C")
       'vedang/notmuch-reply-later)

     ;; Bindings in `notmuch-tree-mode'
     (define-key notmuch-tree-mode-map (kbd "C")
       'vedang/notmuch-reply-later)

     ;; The HEY workflow bindings
     (define-key notmuch-tree-mode-map (kbd "I")
       'vedang/notmuch-move-sender-to-screened)
     ;; Move the existing functionality to a prefix.
     (define-key notmuch-tree-mode-map (kbd "P") nil)
     (define-key notmuch-tree-mode-map (kbd "P p")
       'notmuch-tree-prev-message)
     (define-key notmuch-tree-mode-map (kbd "P l")
       'vedang/notmuch-move-sender-to-papertrail)))

(defun vedang/notmuch-archive-all ()
  "Archive all the emails in the current view."
  (interactive)
  (notmuch-search-archive-thread nil (point-min) (point-max)))

(defun vedang/notmuch-delete-all ()
  "Archive all the emails in the current view.
Mark them for deletion by cron job."
  (interactive)
  (notmuch-search-tag-all '("+deleted"))
  (vedang/notmuch-archive-all))

(defun vedang/notmuch-search-delete-and-archive-thread ()
  "Archive the currently selected thread. Add the deleted tag as well."
  (interactive)
  (notmuch-search-add-tag '("+deleted"))
  (notmuch-search-archive-thread))

(defun vedang/notmuch-tag-and-archive (tag-changes &optional beg end)
  "Prompt the user for TAG-CHANGES.
Apply the TAG-CHANGES to region and also archive all the emails.
When called directly, BEG and END provide the region."
  (interactive (notmuch-search-interactive-tag-changes))
  (notmuch-search-tag tag-changes beg end)
  (notmuch-search-archive-thread nil beg end))

(defun vedang/notmuch-search-get-from ()
  "A helper function to find the email address for the given email."
  (let ((notmuch-addr-sexp (first
                            (notmuch-call-notmuch-sexp "address"
                                                       "--format=sexp"
                                                       "--format-version=1"
                                                       "--output=sender"
                                                       (notmuch-search-find-thread-id)))))
    (plist-get notmuch-addr-sexp :name-addr)))

(defun vedang/notmuch-tree-get-from ()
  "A helper function to find the email address for the given email.
Assumes `notmuch-tree-mode'."
  (plist-get (notmuch-tree-get-prop :headers) :From))

(defun vedang/notmuch-get-from ()
  "Find the From email address for the email at point."
  (car (notmuch-clean-address (cond
                               ((eq major-mode 'notmuch-show-mode)
                                (notmuch-show-get-from))
                               ((eq major-mode 'notmuch-tree-mode)
                                (vedang/notmuch-tree-get-from))
                               ((eq major-mode 'notmuch-search-mode)
                                (vedang/notmuch-search-get-from))
                               ((t nil))))))

(defun vedang/notmuch-filter-by-from ()
  "Filter the current search view to show all emails sent from the sender of the current thread."
  (interactive)
  (notmuch-search-filter (concat "from:" (vedang/notmuch-get-from))))

(defun vedang/notmuch-search-by-from (&optional no-display)
  "Show all emails sent from the sender of the current thread.
NO-DISPLAY is sent forward to `notmuch-search'."
  (interactive)
  (notmuch-search (concat "from:" (vedang/notmuch-get-from))
                  notmuch-search-oldest-first
                  nil
                  nil
                  no-display))

(defun vedang/notmuch-tag-by-from (tag-changes &optional beg end refresh)
  "Apply TAG-CHANGES to all emails from the sender of the current thread.
BEG and END provide the region, but are ignored. They are defined
since `notmuch-search-interactive-tag-changes' returns them. If
REFRESH is true, refresh the buffer from which we started the
search."
  (interactive (notmuch-search-interactive-tag-changes))
  (let ((this-buf (current-buffer)))
    (vedang/notmuch-search-by-from t)
    ;; This is a dirty hack since I can't find a way to run a
    ;; temporary hook on `notmuch-search' completion. So instead of
    ;; waiting on the search to complete in the background and then
    ;; making tag-changes on it, I will just sleep for a short amount
    ;; of time. This is generally good enough and works, but is not
    ;; guaranteed to work every time. I'm fine with this.
    (sleep-for 0.5)
    (notmuch-search-tag-all tag-changes)
    (when refresh
      (set-buffer this-buf)
      (notmuch-refresh-this-buffer))))

(defun vedang/notmuch-add-addr-to-db (nmaddr nmdbfile)
  "Add the email address NMADDR to the db-file NMDBFILE."
  (append-to-file (format "%s\n" nmaddr) nil nmdbfile))

(defun vedang/notmuch-move-sender-to-thefeed (tag-name)
  "For the email at point, move the sender of that email to the feed.
This means:
1. All new email should go to the feed and skip the inbox altogether.
2. All existing email should be updated with the tags =news/TAG-NAME= and =thefeed=.
3. All existing email should be removed from the inbox."
  (interactive "sTag Name: ")
  (vedang/notmuch-add-addr-to-db (format "%s %s"
                                         tag-name
                                         (vedang/notmuch-get-from))
                                 (format "%s/thefeed.db" notmuch-hooks-dir))
  (let ((tag-string (format "+news/%s" tag-name)))
    (vedang/notmuch-tag-by-from (list tag-string "+thefeed" "+archived" "-inbox" "-unread"))))

(defun vedang/notmuch-move-sender-to-papertrail (tag-name)
  "For the email at point, move the sender of that email to the papertrail.
This means:
1. All new email should go to the papertrail and skip the inbox altogether.
2. All existing email should be updated with the tag =ledger/TAG-NAME=.
3. All existing email should be removed from the inbox."
  (interactive "sTag Name: ")
  (vedang/notmuch-add-addr-to-db (format "%s %s"
                                         tag-name
                                         (vedang/notmuch-get-from))
                                 (format "%s/ledger.db" notmuch-hooks-dir))
  (let ((tag-string (format "+ledger/%s" tag-name)))
    (vedang/notmuch-tag-by-from (list tag-string "+archived" "-inbox" "-unread"))))

(defun vedang/notmuch-move-sender-to-screened ()
  "For the email at point, move the sender of that email to Screened Emails.
This means:
1. All new email should be tagged =screened= and show up in the inbox.
2. All existing email should be updated to add the tag =screened=."
  (interactive)
  (vedang/notmuch-add-addr-to-db (vedang/notmuch-get-from)
                                 (format "%s/screened.db" notmuch-hooks-dir))
  (vedang/notmuch-tag-by-from '("+screened")))

(defun vedang/notmuch-move-sender-to-spam ()
  "For the email at point, move the sender of that email to spam.
This means:
1. All new email should go to =spam= and skip the inbox altogether.
2. All existing email should be updated with the tag =spam=.
3. All existing email should be removed from the inbox."
  (interactive)
  (vedang/notmuch-add-addr-to-db (vedang/notmuch-get-from)
                                 (format "%s/spam.db" notmuch-hooks-dir))
  (vedang/notmuch-tag-by-from '("+spam" "+deleted" "+archived" "-inbox" "-unread" "-screened")))

(defun vedang/notmuch-reply-later ()
  "Capture this email for replying later."
  (interactive)
  ;; You need `org-capture' to be set up for this to work. Add this
  ;; code somewhere in your init file after `org-cature' is loaded:

  ;; (push '("r" "Respond to email"
  ;;         entry (file org-default-notes-file)
  ;;         "* TODO Respond to %:from on %:subject  :email: \nSCHEDULED: %t\n%U\n%a\n"
  ;;         :clock-in t
  ;;         :clock-resume t
  ;;         :immediate-finish t)
  ;;       org-capture-templates)

  (org-capture nil "r")

  ;; The rest of this function is just a nice message in the modeline.
  (let* ((email-subject (format "%s..."
                                (substring (notmuch-show-get-subject) 0 15)))
         (email-from (format "%s..."
                             (substring (notmuch-show-get-from) 0 15)))
         (email-string (format "%s (From: %s)" email-subject email-from)))
    (message "Noted! Reply Later: %s" email-string)))

;; Sign messages by default.
(add-hook 'message-setup-hook 'mml-secure-sign-pgpmime)

(setq notmuch-address-selection-function
      (lambda (prompt collection initial-input)
        (completing-read prompt
                         (cons initial-input collection)
                         nil
                         t
                         nil
                         'notmuch-address-history)))

(defun disable-auto-fill ()
  "I don't want `auto-fill-mode'."
  (auto-fill-mode -1))

(add-hook 'message-mode-hook 'disable-auto-fill)

(provide 'init-notmuch)
;;; init-notmuch.el ends here
