;;; rcirc-config.el --- Configuration for RCIRC
;;; Author: Vedang Manerikar
;;; Created on: 04 Mar 2012
;;; Time-stamp: "2012-03-06 19:38:02 vedang"
;;; Copyright (c) 2012 Vedang Manerikar <vedang.manerikar@gmail.com>

;; This file is not part of GNU Emacs.

;;; License:

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the Do What The Fuck You Want to
;; Public License, Version 2, which is included with this distribution.
;; See the file LICENSE.txt

;;; Code:


(require 'rcirc)

;; Don't print /away messages.
;; This does not require rcirc to be loaded already,
;; since rcirc doesn't define a 301 handler (yet).
(defun rcirc-handler-301 (process cmd sender args)
  "/away message handler.")

;; Turn on spell checking.
(add-hook 'rcirc-mode-hook 'turn-on-flyspell)

;; Keep input line at bottom.
(add-hook 'rcirc-mode-hook
          (lambda ()
            (set (make-local-variable 'scroll-conservatively)
                 8192)))

;; Turn on logging everything to a special buffer, for debugging.
(setq rcirc-debug-flag t)

;; Adjust the colours of one of the faces.
(set-face-foreground 'rcirc-my-nick "red" nil)

;; Include date in time stamp.
(setq rcirc-time-format "%Y-%m-%d %H:%M ")

;; Change user info
(setq rcirc-default-nick "h0bbit")
(setq rcirc-default-user-name "vedang")
(setq rcirc-default-full-name "vedang manerikar")

;; Join these channels at startup.
(setq rcirc-startup-channels-alist
      '(("\\.freenode\\.net$" "#emacs" "#git" "#mongodb" "#redis" "#clojure")
        ("\\.qotdinc\\.com$" "#dev" "#bakvaas")))


;;; rcirc/reconnect command - EmacsWiki
(eval-after-load 'rcirc
  '(defun-rcirc-command reconnect (arg)
     "Reconnect the server process."
     (interactive "i")
     (unless process
       (error "There's no process for this target"))
     (let* ((server (car (process-contact process)))
            (port (process-contact process :service))
            (nick (rcirc-nick process))
            channels query-buffers)
       (dolist (buf (buffer-list))
         (with-current-buffer buf
           (when (eq process (rcirc-buffer-process))
             (remove-hook 'change-major-mode-hook
                          'rcirc-change-major-mode-hook)
             (if (rcirc-channel-p rcirc-target)
                 (setq channels (cons rcirc-target channels))
               (setq query-buffers (cons buf query-buffers))))))
       (delete-process process)
       (rcirc-connect server port nick
                      rcirc-default-user-name
                      rcirc-default-full-name
                      channels))))


;;; track activity when I'm in another buffer
(eval-after-load 'rcirc '(rcirc-track-minor-mode 1))


(provide 'rcirc-config)
