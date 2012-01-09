;;; lisp-mode-config.el --- Configuration for Slime and lisp modes in general.
;;; Author: Vedang Manerikar
;;; Created on: 08 Jan 2012
;;; Time-stamp: "2012-01-09 18:22:26 vedang"
;;; Copyright (c) 2012 Vedang Manerikar <vedang.manerikar@gmail.com>

;; This file is not part of GNU Emacs.

;;; License:

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the Do What The Fuck You Want to
;; Public License, Version 2, which is included with this distribution.
;; See the file LICENSE.txt

;;; Code:

(setq slime-net-coding-system 'utf-8-unix)
(slime-setup)
(add-hook 'lisp-mode-hook 'turn-on-slime)
(add-hook 'lisp-mode-hook 'turn-on-paredit)
(add-hook 'inferior-lisp-mode-hook 'turn-on-slime)


(defun turn-on-slime-paredit ()
  "Redefining paredit-space-for-delimiter function so that paredit behaves
well in slime."
  (require 'paredit)
  (defun paredit-space-for-delimiter-p (endp delimiter)
    (and (not (if endp (eobp) (bobp)))
         (memq (char-syntax (if endp (char-after) (char-before)))
               (list ?\" ;; REMOVED ?w ?_
                     (let ((matching (matching-paren delimiter)))
                       (and matching (char-syntax matching)))))))
  (paredit-mode t))


(add-hook 'slime-mode-hook 'turn-on-slime-paredit)
(add-hook 'slime-repl-mode-hook 'turn-on-slime-paredit)
(add-hook 'slime-connected-hook 'turn-on-slime-paredit)


(defadvice slime-repl-emit (after sr-emit-ad activate)
  (with-current-buffer (slime-output-buffer)
    (add-text-properties slime-output-start slime-output-end
                         '(font-lock-face slime-repl-output-face
                                          rear-nonsticky (font-lock-face)))))

(defadvice slime-repl-insert-prompt (after sr-prompt-ad activate)
  (with-current-buffer (slime-output-buffer)
    (let ((inhibit-read-only t))
      (add-text-properties slime-repl-prompt-start-mark (point-max)
                           '(font-lock-face slime-repl-prompt-face
                                            rear-nonsticky
                                            (slime-repl-prompt
                                             read-only
                                             font-lock-face
                                             intangible))))))

;;; Auto complete integration with slime
(require 'ac-slime)
(add-hook 'slime-mode-hook 'set-up-slime-ac)


(provide 'lisp-mode-config)
