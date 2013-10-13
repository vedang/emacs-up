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

(require 'cl-lib)

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


(defun sl/set-auto-major-mode ()
  "When I create a temporary buffer, it should auto-detect the
right mode to start in the buffer."
  (let ((buffer-file-name (or buffer-file-name
                              (buffer-name))))
    (set-auto-mode)))
(setq default-major-mode 'sl/set-auto-major-mode)


(defalias 'yes-or-no-p 'y-or-n-p)


(provide 'site-lisp)
