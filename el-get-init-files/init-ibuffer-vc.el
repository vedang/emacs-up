;;; init-ibuffer-vc.el - Configuration for ibuffer-vc mode
;;; Author: Vedang Manerikar
;;; Created on: 21 Oct 2013
;;; Copyright (c) 2013 Vedang Manerikar <vedang.manerikar@gmail.com>

;; This file is not part of GNU Emacs.

;;; License:

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the Do What The Fuck You Want to
;; Public License, Version 2, which is included with this distribution.
;; See the file LICENSE.txt

;;; Code:;;;


(autoload 'ibuffer-vc-set-filter-groups-by-vc-root "ibuffer-vc"
  "Set ibuffer filter roots by vc root" t)


(eval-after-load "ibuffer"
  '(progn
     (require 'ibuffer-vc)
     (setq ibuffer-default-sorting-mode 'major-mode
           ibuffer-always-show-last-buffer t)
     (define-key ibuffer-mode-map (kbd "C-c C-z")
       'ibuffer-vc-set-filter-groups-by-vc-root)
     (setq ibuffer-formats
           '((mark modified read-only vc-status-mini " "
                   (name 18 18 :left :elide)
                   " "
                   (size 9 -1 :right)
                   " "
                   (mode 16 16 :left :elide)
                   " "
                   (vc-status 16 16 :left)
                   " "
                   filename-and-process)))))


(provide 'init-ibuffer-vc)
