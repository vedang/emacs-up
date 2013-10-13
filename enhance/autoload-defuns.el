;;; autoload-defuns.el - Autoloading and byte-compilation related functions
;;; Author: Vedang Manerikar
;;; Created on: 13 Oct 2013
;;; Copyright (c) 2013 Vedang Manerikar <vedang.manerikar@gmail.com>

;; This file is not part of GNU Emacs.

;;; License:

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the Do What The Fuck You Want to
;; Public License, Version 2, which is included with this distribution.
;; See the file LICENSE.txt

;;; Code:;;;


;;;###autoload
(defun ad/recompile-init (&optional force)
  "Byte-compile all your dotfiles again."
  (interactive "P")
  (byte-recompile-directory dotfiles-dirname 0 force))


(defun ad/el-files-in-dir (directory)
  "List the .el files in DIRECTORY and in it's sub-directories."
  (let* ((current-directory-list (directory-files-and-attributes directory t))
         (el-files-list (delq nil (mapcar (lambda (lst)
                                            (and (equal ".el" (substring (car lst) -3))
                                                 (car lst)))
                                          current-directory-list)))
         (dirs-list (delq nil (mapcar (lambda (lst)
                                        (and (car (cdr lst))
                                             (not (equal "." (substring (car lst) -1)))
                                             (not (equal ".git" (substring (car lst) -4)))
                                             (car lst)))
                                      current-directory-list))))
    (apply #'append el-files-list
           (mapcar (lambda (d)
                     (vedang/el-files-in-dir d))
                   dirs-list))))


(defun ad/update-directory-autoloads (autoload-dir)
  "Update directory autoloads, but better"
  (dolist (el-file (vedang/el-files-in-dir autoload-dir))
    (update-file-autoloads el-file t)))


;;;###autoload
(defun ad/regen-autoloads (&optional force-regen)
  "Regenerate the autoload definitions file if necessary and load it."
  (interactive "P")
  (let ((generated-autoload-file autoload-file))
    (when (or force-regen
              (not (file-exists-p generated-autoload-file)))
      (when (not (file-exists-p generated-autoload-file))
        (with-current-buffer (find-file-noselect generated-autoload-file)
          (insert ";;") ;; create the file with non-zero size to appease autoload
          (save-buffer)))
      (message "Updating autoloads...")
      (dolist (autoload-dir (list plugins-dirname config-dirname elpa-dirname))
        (let (emacs-lisp-mode-hook)
          (vedang/update-directory-autoloads autoload-dir)))))
  (load autoload-file))


(provide 'autoload-defuns)
