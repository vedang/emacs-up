;;; init-recentf.el --- Configuration for recentf
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


(require 'recentf)

(when (not (boundp 'tempfiles-dirname))
  (setq tempfiles-dirname "~/.emacs.d/"))

(setq recentf-exclude (list (concat tempfiles-dirname "*"))
      recentf-save-file (concat tempfiles-dirname ".recentf")
      recentf-max-saved-items 1000
      recentf-max-menu-items 1000
      recentf-menu-filter 'recentf-show-basenames)

(recentf-mode 1)


;; Implement functionality similar to uniquify to make recentf results bearable
;; Requires s.el and dash.el - awesome libraries from Magnar Sven
;; Hat-tip : Baishampayan Ghose for the clojure implementation at
;; https://gist.github.com/ghoseb/8432086
(require 's)
(require 'dash)


(defun explode (d)
  "Explode a directory name to its subcomponents."
  (s-split "/" d))


(defun tails* (coll acc)
  "Return successive tails of a collection."
  (if (cdr coll)
      (tails* (cdr coll) (cons coll acc))
    (cons coll acc)))


(defun tails (coll)
  "Return successive tails of a collection."
  (tails* coll '()))


(defun paths (d)
  "Given a single directory, return all the possible sub-paths / name
  representations for it."
  (mapcar (lambda (xs) (s-join "/" xs)) (tails (explode d))))


(defun index-coll (tab coll)
  "Given a table and a collection, add each entry of the
  collection into the table. If the key already exists, inc it's
  value by 1"
  (mapcar (lambda (x) (puthash x (+ 1 (gethash x tab 0)) tab)) coll)
  tab)


(defun vm-uniquify (filenames)
  "Given a bunch of filenames (as returned by `recentf-list'),
  simplify the names to make them more easily readable."
  (let* ((expanded-paths (mapcar 'paths filenames))
         (tab (make-hash-table :test 'equal))
         (freqs (mapcar (apply-partially 'index-coll tab) expanded-paths)))
    (mapcar (apply-partially '-first (lambda (x) (= 1 (gethash x tab 0))))
            expanded-paths)))


;; Mastering Emacs + some of my own elisp
(defun ido-recentf-open ()
  "Use `ido-completing-read' to \\[find-file] a recent file"
  (interactive)
  (let* ((unique-filenames (vm-uniquify recentf-list))
         (filename-map (-partition 2 (-interleave unique-filenames
                                                  recentf-list)))
         (short-filename (ido-completing-read "Choose recent file: "
                                              unique-filenames
                                              nil
                                              t)))
    (if short-filename
        (find-file (cadr (assoc short-filename filename-map)))
      (message "Aborting"))))

(global-set-key (kbd "C-x C-r") 'ido-recentf-open)


;; Emacswiki
(defsubst file-was-visible-p (file)
  "Return non-nil if FILE's buffer exists and has been displayed."
  (let ((buf (find-buffer-visiting file)))
    (if buf
        (let ((display-count (buffer-local-value 'buffer-display-count buf)))
          (if (> display-count 0) display-count nil)))))

(defsubst keep-default-and-visible-recentf-p (file)
  "Return non-nil if recentf would, by default, keep FILE, and
FILE has been displayed."
  (if (recentf-keep-default-predicate file)
      (file-was-visible-p file)))

;; When a buffer is closed, remove the associated file from the recentf
;; list if (1) recentf would have, by default, removed the file, or
;; (2) the buffer was never displayed.  This is useful because, for
;; example, CEDET opens a lot of files in the background to generate
;; its tags database, etc.
(setq recentf-keep '(keep-default-and-visible-recentf-p))


(defun undo-kill-buffer (arg)
  "Re-open the last buffer killed.  With ARG, re-open the nth buffer."
  (interactive "p")
  (let ((recently-killed-list (copy-sequence recentf-list))
        (buffer-files-list
         (delq nil (mapcar (lambda (buf)
                             (when (buffer-file-name buf)
                               (expand-file-name (buffer-file-name buf)))) (buffer-list)))))
    (mapc
     (lambda (buf-file)
       (setq recently-killed-list
             (delq buf-file recently-killed-list)))
     buffer-files-list)
    (find-file
     (if arg (nth arg recently-killed-list)
       (car recently-killed-list)))))

(provide 'init-recentf)
