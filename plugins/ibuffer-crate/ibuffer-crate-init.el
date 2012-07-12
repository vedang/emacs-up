;;; ibuffer-crate-init.el --- Init file for ibuffer with better settings
;;; Author: Vedang Manerikar
;;; Created on: 11 Jul 2012
;;; Time-stamp: "2012-07-12 10:18:07 vedang"
;;; Copyright (c) 2012 Vedang Manerikar <vedang.manerikar@gmail.com>

;; This file is not part of GNU Emacs.

;;; License:

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the Do What The Fuck You Want to
;; Public License, Version 2, which is included with this distribution.
;; See the file LICENSE.txt

;;; Commentary:
;; To use this code, simply add
;;    (add-to-list 'load-path "/path/to/python-emacs/")
;;    (require 'python-emacs-init)
;; to your .emacs file

;;; Code:


(defun vedang/list-dirs-recursively (directory)
  "List all the directories in DIRECTORY and in it's sub-directories."
  (let* ((current-directory-list (directory-files-and-attributes directory t))
         (dirs-list (delq nil (mapcar (lambda (lst)
                                        (and (car (cdr lst))
                                             (not (equal "." (substring (car lst) -1)))
                                             (not (equal ".git" (substring (car lst) -4)))
                                             (car lst)))
                                      current-directory-list))))
    (apply #'append dirs-list (mapcar (lambda (d)
                                        (vedang/list-dirs-recursively d))
                                      dirs-list))))


(defun vedang/add-dirs-to-load-path (directory)
  "Add all directories under the input directory to the emacs load path"
  (dolist (dirname (vedang/list-dirs-recursively directory))
    (add-to-list 'load-path dirname)))


(vedang/add-dirs-to-load-path (file-name-directory
                               (or (buffer-file-name) load-file-name)))


(require 'ibuffer-mode-config)


(provide 'ibuffer-crate-init)
