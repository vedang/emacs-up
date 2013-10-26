;;; init-rcirc.el --- Configuration for RCIRC
;;; Author: Vedang Manerikar
;;; Created on: 04 Mar 2012
;;; Copyright (c) 2012, 2013 Vedang Manerikar <vedang.manerikar@gmail.com>

;; This file is not part of GNU Emacs.

;;; License:

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the Do What The Fuck You Want to
;; Public License, Version 2, which is included with this distribution.
;; See the file LICENSE.txt

;;; Code:


(setq rcirc-default-nick "h0bbit"
      rcirc-default-user-name "vedang"
      rcirc-default-full-name "vedang manerikar"
      rcirc-debug-flag t
      rcirc-log-directory (concat tempfiles-dirname "rcirc-logs/")
      rcirc-log-flag t
      rcirc-time-format "%Y-%m-%d %H:%M "
      rcirc-keywords '("vedang" "h0bbit_" "bedango")
      rcirc-server-alist '(("irc.freenode.net"
                            :channels ("#emacs" "#git" "#clojure" "#redis"
                                       "#mongodb"))
                           ("irc.qotdinc.com"
                            :channels ("#dev" "#bakvaas" "#nsfw"))))


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


;; Adjust the colours of one of the faces.
(set-face-foreground 'rcirc-my-nick "red" nil)


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
(eval-after-load 'rcirc '(rcirc-track-minor-mode))


(provide 'init-rcirc)
