;;; init-isearch.el --- Configuration for isearch
;;; Author: Vedang Manerikar
;;; Created on: 16 Jan 2012
;;; Copyright (c) 2013 Vedang Manerikar <vedang.manerikar@gmail.com>

;; This file is not part of GNU Emacs.

;;; License:

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the Do What The Fuck You Want to
;; Public License, Version 2, which is included with this distribution.
;; See the file LICENSE.txt

;;; Commentary:
;; From EmacsWiki
;; Move to beginning of word before yanking word in isearch-mode.
;; Make C-s C-w and C-r C-w act like Vim's g* and g#, keeping Emacs'
;; C-s C-w [C-w] [C-w]... behaviour.

;;; Code:

(require 'thingatpt)

(defun is/subst-isearch-yank-word-or-char (&optional use-default?)
  "Activate my customized Isearch word yank command. or
vice-versa. depending on the `use-default?' arg."
  (if use-default?
      (substitute-key-definition 'is/isearch-yank-word-or-char-from-beginning
                                 'isearch-yank-word-or-char
                                 isearch-mode-map)
    (substitute-key-definition 'isearch-yank-word-or-char
                               'is/isearch-yank-word-or-char-from-beginning
                               isearch-mode-map)))

(defun is/isearch-yank-word-or-char-from-beginning (&optional arg)
  "Move to beginning of word before yanking word in isearch-mode."
  (interactive)
  ;; Making this work after a search string is entered by user
  ;; is too hard to do, so work only when search string is empty.
  (if (= 0 (length isearch-string))
      (beginning-of-thing 'word))
  (isearch-yank-word-or-char)
  ;; Revert to 'isearch-yank-word-or-char for subsequent calls
  (is/subst-isearch-yank-word-or-char t))

;; (add-hook 'isearch-mode-hook 'is/subst-isearch-yank-word-or-char)

(defun is/activate-occur ()
  (interactive)
  (let ((case-fold-search isearch-case-fold-search))
    (occur (if isearch-regexp
               isearch-string
             (regexp-quote isearch-string)))))

;; Activate occur easily inside isearch
(with-eval-after-load 'helm-occur
  (define-key isearch-mode-map (kbd "C-o")
              'helm-occur-from-isearch))

;; Use regex searches by default.
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "C-M-s") 'isearch-forward)
(global-set-key (kbd "C-M-r") 'isearch-backward)


(provide 'init-isearch)
