;;; presenting.el --- tools for a simple text-based presentation through -*- lexical-binding: t -*-
;;; Emacs.
;;; Author: Vedang Manerikar
;;; Created on: 13 Oct 2013
;;; Copyright (c) 2013 Vedang Manerikar <vedang.manerikar@gmail.com>

;; This file is not part of GNU Emacs.

;;; License:

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the Do What The Fuck You Want to
;; Public License, Version 2, which is included with this distribution.
;; See the file LICENSE.txt

;; Usage:

;; Each slide should be a seperate file in a single folder. The file
;; names should be 1-Title.whatever, 2-Hello.txt and so on. Start in
;; any file and use the functions `pr/jump-to-next-slide',
;; `pr/jump-to-prev-slide' as needed.

;;; Code:


(defun incs (s &optional num)
  (number-to-string (+ (or num 1) (string-to-number s))))

(defun decs (s &optional num)
  (number-to-string (- (string-to-number s) (or num 1))))


(defun pr/jump-to-next-slide ()
  "Jump to the next slide of the presentation"
  (interactive)
  (condition-case ex
      (find-file (car (file-expand-wildcards (concat
                                              (unhandled-file-name-directory (buffer-file-name))
                                              (incs (car (split-string (file-name-nondirectory (buffer-file-name))
                                                                       "-")))
                                              "-*"))))
    ('error (progn
              (message "Rewinding...")
              (find-file (car (file-expand-wildcards (concat
                                                      (unhandled-file-name-directory (buffer-file-name))
                                                      "1-*"))))))))


(defun pr/jump-to-prev-slide ()
  "Jump to the previous slide of the presentation"
  (interactive)
  (condition-case ex
      (find-file (car (file-expand-wildcards (concat
                                              (unhandled-file-name-directory (buffer-file-name))
                                              (decs (car (split-string (file-name-nondirectory (buffer-file-name))
                                                                       "-")))
                                              "-*"))))
    ('error (message "You've reached the beginning of the presentation"))))


(provide 'presenting)
