;;; site-lisp.el --- Change the behavior of things that come built into
;;; Emacs.
;;; Author: Vedang Manerikar
;;; Created on: 22 Sep 2013
;;; Copyright (c) 2013 Vedang Manerikar <vedang.manerikar@gmail.com>

;; This file is not part of GNU Emacs.

;;; License:

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the Do What The Fuck You Want to
;; Public License, Version 2, which is included with this distribution.
;; See the file LICENSE.txt

;;; Code:;;;

(require 'uniquify)
(setq uniquify-buffer-name-style 'reverse
           uniquify-separator "|"
           uniquify-after-kill-buffer-p t
           uniquify-ignore-buffers-re "^\\*")

(require 'saveplace)


;; customizations for auto-indentation
(defadvice yank (after indent-region activate)
  (if (member major-mode vedang/programming-major-modes)
      (let ((mark-even-if-inactive t))
        (indent-region (region-beginning) (region-end) nil))))

(defadvice yank-pop (after indent-region activate)
  (if (member major-mode vedang/programming-major-modes)
      (let ((mark-even-if-inactive t))
        (indent-region (region-beginning) (region-end) nil))))


;; when I create a temporary buffer, it should auto-detect the right
;; mode to start in the buffer
(setq default-major-mode (lambda ()
                           (let ((buffer-file-name (or buffer-file-name
                                                       (buffer-name))))
                             (set-auto-mode))))


;; Close emacsclient buffers using C-x k
(add-hook 'server-switch-hook
          (lambda ()
            (when (current-local-map)
              (use-local-map (copy-keymap (current-local-map))))
            (when server-buffer-clients
              (local-set-key (kbd "C-x k") 'server-edit))))


(defalias 'yes-or-no-p 'y-or-n-p)


(provide 'site-lisp)
