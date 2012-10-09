;;; auto-complete-mode-config.el --- configuration for auto-complete-mode-mode
;;; Author: Vedang Manerikar
;;; Created on: 16 Jan 2012
;;; Time-stamp: "2012-10-09 16:14:47 vedang"
;;; Copyright (c) 2012 Vedang Manerikar <vedang.manerikar@gmail.com>

;; This file is not part of GNU Emacs.

;;; License:

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the Do What The Fuck You Want to
;; Public License, Version 2, which is included with this distribution.
;; See the file LICENSE.txt

;;; Code:


(require 'auto-complete-config)

(add-to-list 'ac-dictionary-directories
             (concat *plugins-dir* "auto-complete/ac-dict"))

(ac-config-default) ; use the default sources, they are good
(ac-flyspell-workaround) ; stop autocomplete from clashing with flyspell
(global-auto-complete-mode t) ; use autocomplete everywhere
(setq ac-auto-show-menu 0.8) ; Show auto-complete menu 0.8 second later

(set-default 'ac-sources
             '(ac-source-dictionary
               ac-source-words-in-buffer
               ac-source-words-in-same-mode-buffers
               ac-source-words-in-all-buffer
               ac-source-yasnippet))

(dolist (mode '(magit-log-edit-mode log-edit-mode org-mode text-mode html-mode
                nxml-mode clojure-mode lisp-mode textile-mode markdown-mode
                objc-mode))
  (add-to-list 'ac-modes mode))

(setq ac-use-menu-map t)
(setq ac-comphist-file (concat *tempfiles-dir* "ac-comphist.dat"))
(define-key ac-menu-map "\C-n" 'ac-next)
(define-key ac-menu-map "\C-p" 'ac-previous)

(provide 'auto-complete-mode-config)
