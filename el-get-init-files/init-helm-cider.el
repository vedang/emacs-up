;;; init-helm-cider.el --- Configuration for helm Cider.
;;; Commentary:
;;; Author: Vedang Manerikar
;;; Created on: 24 Oct 2016
;;; Copyright (c) 2013, 2014, 2015, 2016 Vedang Manerikar <vedang.manerikar@gmail.com>

;; This file is not part of GNU Emacs.

;;; License:

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the Do What The Fuck You Want to
;; Public License, Version 2, which is included with this distribution.
;; See the file LICENSE.txt

;;; Code:


(eval-after-load 'cider-mode
  '(progn (helm-cider-mode 1)
          (setq helm-cider-apropos-actions
                '(("Find definition" lambda
                   (candidate)
                   (cider-find-var nil candidate))
                  ("CiderDoc" . cider-doc-lookup)
                  ("Find on Grimoire" . cider-grimoire-lookup)))
          ;; define keys for apropos that follow helm conventions
          (define-key cider-mode-map (kbd "C-x c d n") 'cider-browse-ns)
          (define-key cider-mode-map (kbd "C-x c d a") 'cider-apropos)
          (define-key cider-mode-map (kbd "C-x c d e") 'cider-apropos-documentation)))

(provide 'init-helm-cider)
;;; init-helm-cider ends here
