;;; extra-bindings.el --- convenience bindings for various things.
;;; Author: Vedang Manerikar
;;; Created on: 13 Oct 2013
;;; Copyright (c) 2013 -- 2021 Vedang Manerikar <vedang.manerikar@gmail.com>
;;; Commentary:
;; No commentary at the moment

;; This file is not part of GNU Emacs.

;;; License:

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the Do What The Fuck You Want to
;; Public License, Version 2, which is included with this distribution.
;; See the file LICENSE.txt

;;; Code:

(require 'utility-functions)
(global-set-key (kbd "C-w") 'uf/backward-kill-word-or-kill-region)
(global-set-key (kbd "C-M-y") #'uf/reverse-transpose-sexps)
(global-set-key (kbd "C-x M-s") 'uf/transpose-windows)

(global-set-key (kbd "A-q") 'uf/unfill-paragraph)

(require 'presenting)
(global-set-key (kbd "<f7>") 'pr/jump-to-prev-slide)
(global-set-key (kbd "<f8>") 'pr/jump-to-next-slide)

;; Custom 'apropos' key bindings
;; http://www.masteringemacs.org/articles/2011/08/04/full-text-searching-info-mode-apropos/#comment-1409
(global-set-key (kbd "C-h a") 'Apropos-Prefix)
(define-prefix-command 'Apropos-Prefix nil "Apropos (a,d,f,i,l,v,C-v)")
(define-key Apropos-Prefix (kbd "a")   'apropos)
(define-key Apropos-Prefix (kbd "d")   'apropos-documentation)
(define-key Apropos-Prefix (kbd "f")   'apropos-command)
(define-key Apropos-Prefix (kbd "c")   'apropos-command)
(define-key Apropos-Prefix (kbd "i")   'info-apropos)
(define-key Apropos-Prefix (kbd "l")   'apropos-library)
(define-key Apropos-Prefix (kbd "v")   'apropos-variable)
(define-key Apropos-Prefix (kbd "C-v") 'apropos-value)


;; Stop IELM from being stupid
(with-eval-after-load 'ielm
  (define-key ielm-map (kbd "C-j") 'newline-and-indent))

;; Keybindings for notmuch
(global-set-key (kbd "<f10>") 'notmuch)

;;; Use ~C-c C-o~ as binding for `goto-address-at-point' to mimic
;;; org-mode behaviour for opening links.
(with-eval-after-load 'goto-addr
  (define-key goto-address-highlight-keymap (kbd "C-c C-o")
              'goto-address-at-point))

(with-eval-after-load 'shr
  (define-key shr-map (kbd "C-c C-o") 'shr-browse-url))

(provide 'extra-bindings)
