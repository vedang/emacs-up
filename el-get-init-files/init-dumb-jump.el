;;; init-dumb-jump.el --- Configuration for dump-jump mode
;;; Commentary:
;;; Author: Vedang Manerikar
;;; Created on: 8th Oct 2019
;;; Copyright (c) 2019 Vedang Manerikar <vedang.manerikar@gmail.com>

;; This file is not part of GNU Emacs.

;;; License:

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the Do What The Fuck You Want to
;; Public License, Version 2, which is included with this distribution.
;; See the file LICENSE.txt

;;; Code:
(dumb-jump-mode)

(define-key dumb-jump-mode-map (kbd "C-c d g")
  'dumb-jump-go)
(define-key dumb-jump-mode-map (kbd "C-c d b")
  'dumb-jump-back)
(define-key dumb-jump-mode-map (kbd "C-c d q")
  'dumb-jump-quick-look)

(define-key dumb-jump-mode-map (kbd "C-M-q") nil)
;;(setq dumb-jump-selector 'helm)

;; (defun dumb-jump--xref-backend () 'dumb-jump)

;; (cl-defmethod xref-backend-identifier-at-point ((_backend (eql dumb-jump)))
;;   (find-tag--default))

;; (cl-defmethod xref-backend-identifier-completion-table ((_backend (eql dumb-jump)))
;;   (tags-lazy-completion-table))

;; (cl-defmethod xref-backend-definitions ((_backend (eql dumb-jump)) symbol)
;;   (etags--xref-find-definitions symbol))

;; (cl-defmethod xref-backend-apropos ((_backend (eql dumb-jump)) symbol)
;;   (etags--xref-find-definitions symbol t))

;; (defun etags--xref-find-definitions (pattern &optional regexp?)
;;   ;; This emulates the behavior of `find-tag-in-order' but instead of
;;   ;; returning one match at a time all matches are returned as list.
;;   ;; NOTE: find-tag-tag-order is typically a buffer-local variable.
;;   (let* ((xrefs '())
;;          (first-time t)
;;          (search-fun (if regexp? #'re-search-forward #'search-forward))
;;          (marks (make-hash-table :test 'equal))
;;          (case-fold-search (if (memq tags-case-fold-search '(nil t))
;;                                tags-case-fold-search
;;                              case-fold-search))
;;          (cbuf (current-buffer)))
;;     (save-excursion
;;       (while (visit-tags-table-buffer (not first-time) cbuf)
;;         (setq first-time nil)
;;         (dolist (order-fun (cond (regexp? find-tag-regexp-tag-order)
;;                                  (t etags-xref-find-definitions-tag-order)))
;;           (goto-char (point-min))
;;           (while (and (funcall search-fun pattern nil t)
;;                       (< (hash-table-count marks) etags--xref-limit))
;;             (when (funcall order-fun pattern)
;;               (beginning-of-line)
;;               (pcase-let* ((tag-info (etags-snarf-tag))
;;                            (`(,hint ,line . _) tag-info))
;;                 (unless (eq hint t) ; hint==t if we are in a filename line
;;                   (let* ((file (file-of-tag))
;;                          (mark-key (cons file line)))
;;                     (unless (gethash mark-key marks)
;;                       (let ((loc (xref-make-etags-location
;;                                   tag-info (expand-file-name file))))
;;                         (push (xref-make hint loc) xrefs)
;;                         (puthash mark-key t marks)))))))))))
;;     (nreverse xrefs)))

;; (defclass xref-etags-location (xref-location)
;;   ((tag-info :type list   :initarg :tag-info)
;;    (file     :type string :initarg :file
;;              :reader xref-location-group))
;;   :documentation "Location of an etags tag.")

;; (defun xref-make-etags-location (tag-info file)
;;   (make-instance 'xref-etags-location :tag-info tag-info
;;                  :file (expand-file-name file)))

;; (cl-defmethod xref-location-marker ((l xref-etags-location))
;;   (with-slots (tag-info file) l
;;     (let ((buffer (find-file-noselect file)))
;;       (with-current-buffer buffer
;;         (save-excursion
;;           (etags-goto-tag-location tag-info)
;;           (point-marker))))))

;; (cl-defmethod xref-location-line ((l xref-etags-location))
;;   (with-slots (tag-info) l
;;     (nth 1 tag-info)))

(provide 'init-dumb-jump)
;;; init-dumb-jump.el ends here
