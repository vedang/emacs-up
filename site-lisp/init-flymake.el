;;; init-flymake.el --- Configuration for flymake
;;; Author: Vedang Manerikar
;;; Created on: 16 Jan 2012
;;; Copyright (c) 2013 Vedang Manerikar <vedang.manerikar@gmail.com>

;; This file is not part of GNU Emacs.

;;; License:

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the Do What The Fuck You Want to
;; Public License, Version 2, which is included with this distribution.
;; See the file LICENSE.txt

;;; Code:


;; I don't want flymake for these modes.
(defvar flymake-disabled-modes '("java" "xml" "html"))


(defun if/flymake-delete-unnecessary-modes (modes-to-disable)
  "Remove modes from flymake-allowed-file-name-masks to avoid flymake popup."
  (let* ((flymake-elts-to-delete))
    (dolist (elt flymake-allowed-file-name-masks)
      (dolist (disabled-mode modes-to-disable)
        (when (eq (string-match-p (car elt) (concat "." disabled-mode)) 0)
          (add-to-list 'flymake-elts-to-delete elt))))
    (dolist (elt flymake-elts-to-delete)
      (setq flymake-allowed-file-name-masks
            (delete elt flymake-allowed-file-name-masks)))))


(eval-after-load "flymake"
  '(progn
     (require 'flymake-cursor)
     (global-set-key (kbd "<f3>") 'flymake-goto-prev-error)
     (global-set-key (kbd "<f2>") 'flymake-goto-next-error)

     (if/flymake-delete-unnecessary-modes flymake-disabled-modes)))


(provide 'init-flymake)
