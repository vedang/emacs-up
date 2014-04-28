;;; init-el-get.el - El-get for Great Good.
;;; Author: Vedang Manerikar
;;; Created on: 22 Dec 2013
;;; Copyright (c) 2013 Vedang Manerikar <vedang.manerikar@gmail.com>

;; This file is not part of GNU Emacs.

;;; License:

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the Do What The Fuck You Want to
;; Public License, Version 2, which is included with this distribution.
;; See the file LICENSE.txt

;;; Code:

(setq el-get-dirname (concat dotfiles-dirname "el-get/")
      el-get-user-package-directory (concat dotfiles-dirname
                                            "el-get-init-files/")
      el-get-my-recipes (concat el-get-user-package-directory
                                "personal-recipes/"))

(add-to-list 'load-path (concat el-get-dirname "el-get"))

(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.github.com/dimitri/el-get/master/el-get-install.el")
    (let (el-get-master-branch
          el-get-install-skip-emacswiki-recipes)
      (goto-char (point-max))
      (eval-print-last-sexp))))

(add-to-list 'el-get-recipe-path el-get-my-recipes)

;; Tie volatile stuff down, so that configuration does not break.
(setq el-get-sources
      '((:name cider
               ;; Previously working checkout
               ;; :checkout "b1c162cf691bd850153c0a7a9b75be8b04984641"
               :checkout "9a5b389c2acbf5a9bb4c340bb0907cce9fcaf1b8"
               )
        (:name writegood
               :after (progn (global-set-key (kbd "C-c g") 'writegood-mode)))))

(defvar el-get-my-packages (append
                            '(ac-nrepl
                              ace-jump-mode
                              auto-complete
                              clojure-mode
                              clj-refactor
                              color-theme-zenburn
                              dash
                              el-spice
                              flymake-cursor
                              ibuffer-vc
                              magit
                              markdown-mode
                              multiple-cursors
                              org-mode
                              org-mode-crate
                              paredit
                              s
                              smart-tab
                              smex
                              unbound
                              wgrep
                              yasnippet)
                            (mapcar 'el-get-source-name el-get-sources)))

(when on-my-machine
  ;; Load packages with Third Party dependencies only on my machine.
  (setq el-get-my-packages (append '(emacs-eclim) el-get-my-packages)))

(el-get 'sync el-get-my-packages)

(provide 'init-el-get)
