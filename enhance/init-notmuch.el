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
      sendmail-program "/usr/local/bin/msmtp")

(provide 'init-notmuch)
