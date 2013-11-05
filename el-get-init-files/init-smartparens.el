;;; init-smartparens.el - Configuration for smartparens mode
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


(require 'smartparens-config)

(smartparens-global-strict-mode)
(show-smartparens-global-mode)
(sp-use-paredit-bindings)

(sp-pair "(" nil :insert "C-o")

(setq sp-hybrid-kill-entire-symbol nil)


;; Keybindings

;; Unbind `M-s' (set by paredit keybindings above) because it's bound
;; to some handy occur related functions
(define-key sp-keymap (kbd "M-s") nil)

;; Other key-bindings - apart from paredit key-bindings.
;; Taken from the Smartparens wiki page.
(define-key sp-keymap (kbd "C-M-a") 'sp-backward-down-sexp)
(define-key sp-keymap (kbd "C-S-a") 'sp-beginning-of-sexp)
(define-key sp-keymap (kbd "C-S-d") 'sp-end-of-sexp)

(define-key sp-keymap (kbd "C-M-e") 'sp-up-sexp)
(define-key emacs-lisp-mode-map (kbd ")") 'sp-up-sexp)
(define-key sp-keymap (kbd "C-M-t") 'sp-transpose-sexp)

(define-key sp-keymap (kbd "C-M-n") 'sp-next-sexp)
(define-key sp-keymap (kbd "C-M-p") 'sp-previous-sexp)

(define-key sp-keymap (kbd "C-M-k") 'sp-kill-sexp)
(define-key sp-keymap (kbd "C-M-w") 'sp-copy-sexp)

(define-key sp-keymap (kbd "M-<delete>") 'sp-unwrap-sexp)
(define-key sp-keymap (kbd "M-<backspace>") 'sp-backward-unwrap-sexp)

(define-key sp-keymap (kbd "M-D") 'sp-splice-sexp)
(define-key sp-keymap (kbd "C-M-]") 'sp-select-next-thing-exchange)
(define-key sp-keymap (kbd "C-]") 'sp-select-next-thing)

(define-key sp-keymap (kbd "M-F") 'sp-forward-symbol)
(define-key sp-keymap (kbd "M-B") 'sp-backward-symbol)

(define-key sp-keymap (kbd "s-t") 'sp-prefix-tag-object)
(define-key sp-keymap (kbd "s-p") 'sp-prefix-pair-object)
(define-key sp-keymap (kbd "s-s c") 'sp-convolute-sexp)
(define-key sp-keymap (kbd "s-s a") 'sp-absorb-sexp)
(define-key sp-keymap (kbd "s-s e") 'sp-emit-sexp)
(define-key sp-keymap (kbd "s-s p") 'sp-add-to-previous-sexp)
(define-key sp-keymap (kbd "s-s n") 'sp-add-to-next-sexp)
(define-key sp-keymap (kbd "s-s j") 'sp-join-sexp)
(define-key sp-keymap (kbd "s-s s") 'sp-split-sexp)


(provide 'init-smartparens)
