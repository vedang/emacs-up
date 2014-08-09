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
(defvar flymake-disabled-modes '("java" "xml" "html" "c"))


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


;; http://git.vo20.nl/
;; Fix for nasty `cannot open doc string file` flymake error
(defun if/flymake-create-temp-intemp (file-name prefix)
  "Return file name in temporary directory for checking FILE-NAME.
  This is a replacement for `flymake-create-temp-inplace'. The
  difference is that it gives a file name in
  `temporary-file-directory' instead of the same directory as
  FILE-NAME.

  For the use of PREFIX see that function.

  Note that not making the temporary file in another directory
  \(like here) will not if the file you are checking depends on
  relative paths to other files \(for the type of checks flymake
  makes)."
  (unless (stringp file-name) (error "Invalid file-name"))
  (or prefix (setq prefix "flymake"))
  (let* ((name (concat
                (file-name-nondirectory
                 (file-name-sans-extension file-name))
                "_" prefix))
         (ext  (concat "." (file-name-extension file-name)))
         (temp-name (make-temp-file name nil ext)))
    (flymake-log 3 "create-temp-intemp: file=%s temp=%s" file-name temp-name)
    temp-name))


;; Flymake for java.
;; Taken from the configuration of tkj
;; https://github.com/skybert/my-little-friends
(defun if/flymake-javachecks-init ()
  (list "flymake-java-checker"
        (list (flymake-init-create-temp-buffer-copy
               'flymake-create-temp-with-folder-structure))))


(eval-after-load "flymake"
  '(progn
     (require 'flymake-cursor)
     (global-set-key (kbd "<f3>") 'flymake-goto-prev-error)
     (global-set-key (kbd "<f2>") 'flymake-goto-next-error)
     (global-set-key (kbd "C-c C-p") 'flymake-goto-prev-error)
     (global-set-key (kbd "C-c C-n") 'flymake-goto-next-error)
     (if/flymake-delete-unnecessary-modes flymake-disabled-modes)

     ;; (add-to-list 'flymake-allowed-file-name-masks
     ;;              '("\\.java\\'"
     ;;                if/flymake-javachecks-init
     ;;                flymake-simple-cleanup))
     ))

(provide 'init-flymake)
