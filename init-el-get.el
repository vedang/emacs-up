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
               ;; :checkout "90c226a48e5db04c7bc28ce5df2a32fa228fce55"
               :checkout "bdcfceb2956c9137ef355afbb637a85bb249dd1d")
        (:name writegood
               :post-init (progn
                            (global-set-key (kbd "C-c g") 'writegood-mode)))))

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
                              s
                              smart-tab
                              smartparens
                              smex
                              unbound
                              wgrep
                              ;; workgroups - don't find this so useful.
                              yasnippet)
                            (mapcar 'el-get-source-name el-get-sources)))

(when on-my-machine
  ;; Load packages with Third Party dependencies only on my machine.
  (setq el-get-my-packages (append '(emacs-eclim) el-get-my-packages)))

(el-get 'sync el-get-my-packages)

(provide 'init-el-get)
