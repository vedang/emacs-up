;;; init-notmuch.el - configuration for using notmuch to manage email
;;; Author: Vedang Manerikar
;;; Created on: 07th June 2014
;;; Copyright (c) 2014 Vedang Manerikar <vedang.manerikar@gmail.com>

;; This file is not part of GNU Emacs.

;;; License:

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the Do What The Fuck You Want to
;; Public License, Version 2, which is included with this distribution.
;; See the file LICENSE.txt

;;; Code:


(setq user-mail-address (notmuch-user-primary-email)
      user-full-name (notmuch-user-name)
      message-send-mail-function 'message-send-mail-with-sendmail
      ;; we substitute sendmail with msmtp
      sendmail-program (executable-find "msmtp")
      message-sendmail-envelope-from 'header
      mail-specify-envelope-from t
      notmuch-archive-tags '("-inbox" "-unread" "+archived")
      notmuch-search-oldest-first nil
      notmuch-show-indent-content nil)

(defun vedang/notmuch-archive-all ()
  "Archive all the emails in the current view."
  (interactive)
  (notmuch-search-archive-thread nil (point-min) (point-max)))

(defun vedang/notmuch-delete-all ()
  "Archive all the emails in the current view. Mark them for
deletion by cron job."
  (interactive)
  (notmuch-search-tag-all '("+deleted"))
  (vedang/notmuch-archive-all))

(defun vedang/notmuch-search-delete-and-archive-thread ()
  "Archive the currently selected thread. Add the deleted tag as well."
  (interactive)
  (notmuch-search-add-tag '("+deleted"))
  (notmuch-search-archive-thread))

(defun vedang/notmuch-filter-by-from ()
  "Filter the current search view to show all emails sent from the sender of the current thread."
  (interactive)
  (let* ((notmuch-addr-sexp (first
                             (notmuch-call-notmuch-sexp "address"
                                                        "--format=sexp"
                                                        "--format-version=1"
                                                        "--output=sender"
                                                        (notmuch-search-find-thread-id))))
         (from-addr (plist-get notmuch-addr-sexp :address)))
    (notmuch-search-filter (concat "from:" from-addr))))

(defun vedang/notmuch-search-by-from ()
  "Show all emails sent from the sender of the current thread."
  (interactive)
  (let* ((notmuch-addr-sexp (first
                             (notmuch-call-notmuch-sexp "address"
                                                        "--format=sexp"
                                                        "--format-version=1"
                                                        "--output=sender"
                                                        (notmuch-search-find-thread-id))))
         (from-addr (plist-get notmuch-addr-sexp :address)))
    (notmuch-search (concat "from:" from-addr))))

;;; EmacsWiki
(defun my-notmuch-show-view-as-patch ()
  "View the the current message as a patch."
  (interactive)
  (let* ((id (notmuch-show-get-message-id))
         (subject (concat "Subject: " (notmuch-show-get-subject) "\n"))
         (diff-default-read-only t)
         (buf (get-buffer-create (concat "*notmuch-patch-" id "*")))
         (map (make-sparse-keymap)))
    (define-key map "q" 'notmuch-kill-this-buffer)
    (switch-to-buffer buf)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert subject)
      (insert (notmuch-get-bodypart-internal id 1 nil)))
    (set-buffer-modified-p nil)
    (diff-mode)
    (lexical-let ((new-ro-bind (cons 'buffer-read-only map)))
      (add-to-list 'minor-mode-overriding-map-alist new-ro-bind))
    (goto-char (point-min))))

(eval-after-load 'notmuch-show
  '(progn (define-key notmuch-show-mode-map (kbd "r")
            'notmuch-show-reply)
          (define-key notmuch-show-mode-map (kbd "R")
            'notmuch-show-reply-sender)
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
          (define-key 'notmuch-show-mode-map (kbd "D")
            'my-notmuch-show-view-as-patch)))

;; Sign messages by default.
(add-hook 'message-setup-hook 'mml-secure-sign-pgpmime)

;; Integrate with org-mode
(require 'ol-notmuch)

(add-to-list 'notmuch-saved-searches
             '(:name "hs-inbox"
                     :query "tag:inbox AND (to:vedang@helpshift.com OR from:helpshift.com)"
                     :count-query "tag:inbox AND (to:vedang@helpshift.com OR from:helpshift.com) AND tag:unread"
                     :key "h"))
(add-to-list 'notmuch-saved-searches
             '(:name "hs-archive"
                     :query "tag:inbox AND from:bots@helpshift.com"
                     :key "H"))
(add-to-list 'notmuch-saved-searches
             '(:name "non-jira hs-inbox"
                     :query "tag:inbox AND (to:vedang@helpshift.com OR from:helpshift.com) AND NOT from:jira@helpshift.atlassian.net"
                     :key "j"))
(add-to-list 'notmuch-saved-searches
             '(:name "only-jira"
                     :query "tag:inbox AND (to:vedang@helpshift.com OR from:helpshift.com) AND from:jira@helpshift.atlassian.net"
                     :key "J"))
(add-to-list 'notmuch-saved-searches
             '(:name "hsbug"
                     :query "tag:inbox AND (to:vedang@helpshift.com OR from:helpshift.com) AND from:jira@helpshift.atlassian.net AND HSBUG"
                     :key "B"))
(add-to-list 'notmuch-saved-searches
             '(:name "non-helpshift-inbox"
                     :query "tag:inbox AND NOT to:helpshift.com"
                     :key "I"))


(setq notmuch-address-selection-function
      (lambda (prompt collection initial-input)
        (completing-read prompt
                         (cons initial-input collection)
                         nil
                         t
                         nil
                         'notmuch-address-history)))

(defun disable-auto-fill ()
  "I don't want `auto-fill-mode'"
  (auto-fill-mode -1))

(add-hook 'message-mode-hook 'disable-auto-fill)

(provide 'init-notmuch)
;;; init-notmuch.el ends here
