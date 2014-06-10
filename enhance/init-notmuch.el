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
      sendmail-program "/usr/local/bin/msmtp"
      notmuch-archive-tags '("-inbox" "-unread" "+archived"))

(defun vedang/notmuch-archive-all ()
  "Archive all the emails in the current view."
  (interactive)
  (notmuch-search-archive-thread nil (point-min) (point-max)))

(define-key notmuch-show-mode-map (kbd "r") 'notmuch-show-reply)
(define-key notmuch-show-mode-map (kbd "R") 'notmuch-show-reply-sender)
(define-key notmuch-search-mode-map (kbd "r") 'notmuch-search-reply-to-thread)
(define-key notmuch-search-mode-map (kbd "R") 'notmuch-search-reply-to-thread-sender)
(define-key notmuch-search-mode-map (kbd "A") 'vedang/notmuch-archive-all)

;; Sign messages by default.
(add-hook 'message-setup-hook 'mml-secure-sign-pgpmime)

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

(define-key 'notmuch-show-mode-map (kbd "D") 'my-notmuch-show-view-as-patch)

(provide 'init-notmuch)
