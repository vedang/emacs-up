;;; mode-config.el --- Main config file for the various emacs modes I use
;;; Author: Vedang Manerikar
;;; Created on: 08 Jan 2012
;;; Time-stamp: "2012-01-16 21:25:55 vedang"
;;; Copyright (c) 2012 Vedang Manerikar <vedang.manerikar@gmail.com>

;; This file is not part of GNU Emacs.

;;; License:

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the Do What The Fuck You Want to
;; Public License, Version 2, which is included with this distribution.
;; See the file LICENSE.txt

;;; Code:


(autoload 'package-list-packages "elpa-config" "List Elpa packages" t)
(autoload 'cscope-set-initial-directory "cscope-mode-config" "Load cscope" t)
(autoload 'python-mode "python" "Load python mode" t)
(autoload 'no-easy-keys-minor-mode "no-easy-keys" "Load no easy keys" t)
(autoload 'octave-mode "octave-mode" nil t)
(autoload 'xmsi-mode "xmsi-math-symbols-input.el"
  "Load xmsi-math-symbols-input mode for inserting math symbols." t)
(autoload 'writegood-mode "writegood-mode" "Write Good." t)

;;; required magic
(require 'ido-mode-config)
(require 'ibuffer-mode-config)
(require 'auto-complete-mode-config)
(require 'revive-mode-config)
(require 'isearch-mode-config)
(require 'flymake-config)
(require 'erlang-start)
(require 'template)
(template-initialize)

;;; Eval after loads
(eval-after-load "org"
  '(progn
     (require 'org-mode-config)))
(eval-after-load "erc"
  '(progn
     (require 'erc-mode-config)))
(eval-after-load 'diff-mode
  '(progn
     (set-face-foreground 'diff-added "green4")
     (set-face-foreground 'diff-removed "red3")))
(eval-after-load 'magit
  '(progn
     (set-face-foreground 'magit-diff-add "green3")
     (set-face-foreground 'magit-diff-del "red3")))
(eval-after-load "python"
  '(progn
     (require 'python-mode-config)))
(eval-after-load "erlang"
  '(progn
     (require 'erlang-mode-config)))
(eval-after-load "lisp-mode"
  '(progn
     (require 'emacs-lisp-mode-config)))
(eval-after-load "clojure"
  '(progn
     (require 'clojure-mode-config)))
(eval-after-load "slime"
  '(progn
     (require 'slime-config)))
(eval-after-load "LaTeX"
  '(progn
     (require 'latex-mode-config)))
(eval-after-load 'uniquify
  '(progn
     (setq uniquify-buffer-name-style 'reverse
           uniquify-separator "/"
           uniquify-after-kill-buffer-p t
           uniquify-ignore-buffers-re "^\\*")))


;;; configuration too small to go into individual files
(require 'yasnippet) ;; not yasnippet-bundle
(global-set-key (kbd "<s-tab>") 'yas/trigger-key)
(yas/initialize)
(yas/load-directory "~/.emacs.d/plugins/yasnippet/snippets")


(setq hippie-expand-try-functions-list
      '(yas/hippie-try-expand
        try-expand-dabbrev
        try-expand-dabbrev-from-kill
        try-expand-dabbrev-all-buffers
        try-complete-file-name-partially
        try-complete-file-name
        try-complete-lisp-symbol-partially
        try-complete-lisp-symbol))


(add-hook 'octave-mode-hook
          (lambda ()
            (abbrev-mode 1)
            (auto-fill-mode 1)
            (if (eq window-system 'x)
                (font-lock-mode 1))))


(add-hook 'inferior-octave-mode-hook
          (lambda ()
            (turn-on-font-lock)
            (define-key inferior-octave-mode-map [up]
              'comint-previous-input)
            (define-key inferior-octave-mode-map [down]
              'comint-next-input)))


(provide 'mode-config)
