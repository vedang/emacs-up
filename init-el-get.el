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

(setq el-get-dir (concat dotfiles-dirname "el-get/")
      el-get-user-package-directory (concat dotfiles-dirname
                                            "el-get-init-files/")
      el-get-my-recipes (concat el-get-user-package-directory
                                "personal-recipes/"))

(add-to-list 'load-path (concat el-get-dir "el-get"))

(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.github.com/dimitri/el-get/master/el-get-install.el")
    (let (el-get-master-branch
          el-get-install-skip-emacswiki-recipes)
      (goto-char (point-max))
      (eval-print-last-sexp))))

(add-to-list 'el-get-recipe-path el-get-my-recipes)

;;; This is the order in which the packages are loaded. Changing this
;;; order can sometimes lead to nasty surprises, especially when you
;;; are overshadowing some in-built libraries. *cough*org-mode*cough*
(when (eq system-type 'darwin)
  (el-get 'sync '(exec-path-from-shell)))

;; Tie volatile stuff down, so that configuration does not break.
;; Add configuration for recipes that need very minor configuration.
(setq el-get-sources
      (append
       (when (and (boundp configure-clojure-p)
                  configure-clojure-p)
         '((:name cider
                  :checkout "v0.12.0")
           (:name clj-refactor
                  ;; v2.2.0 is untagged
                  :checkout "531a09f")
           (:name flycheck-clojure
                  :after (progn ;; (eval-after-load 'flycheck
                                ;;   '(flycheck-clojure-setup))
                                ))))
       (when (and (boundp configure-python-p)
                  configure-python-p)
         '((:name elpy
                  :after (progn (elpy-enable)))))
       (when (and (boundp configure-rust-p)
                  configure-rust-p)
         (if (executable-find "rustc")
             '((:name rust-mode
                      :after (progn (add-to-list 'auto-mode-alist
                                                 '("\\.rs\\'" . rust-mode)))))
           (error "Rust Lang programming is configured, but I can't find the `rustc' binary! Have you read the README file?")))
       '((:name change-inner
                :after (progn (global-set-key (kbd "M-i") 'change-inner)
                              (global-set-key (kbd "M-o") 'change-outer)))
         (:name dash-at-point
                :after (progn (global-set-key (kbd "C-c d") 'dash-at-point)))
         (:name expand-region
                :after (progn (global-set-key (kbd "C-=") 'er/expand-region)))
         (:name es-mode
                :after (progn (add-to-list 'auto-mode-alist
                                           '("\\.es$" . es-mode))
                              (setq es-always-pretty-print t)))
         (:name flycheck
                :after (progn (global-flycheck-mode 1)))
         (:name flycheck-pos-tip
                :after (progn (eval-after-load 'flycheck
                                '(progn (require 'flycheck-pos-tip)
                                        (setq flycheck-display-errors-function
                                              #'flycheck-pos-tip-error-messages)))))
         (:name smart-tab
                :after (progn (setq smart-tab-using-hippie-expand t)
                              (global-smart-tab-mode 1)))
         (:name unicode-fonts
                :after (progn (unicode-fonts-setup)))
         (:name writegood
                :after (progn (global-set-key (kbd "C-c g") 'writegood-mode)))
         (:name xterm-color
                :after (progn (add-hook 'comint-preoutput-filter-functions 'xterm-color-filter)
                              (setq comint-output-filter-functions (remove 'ansi-color-process-output comint-output-filter-functions))
                              (setq font-lock-unfontify-region-function 'xterm-color-unfontify-region)))
         (:name yasnippet
                :after (progn (yas-global-mode 1)
                              (add-to-list 'hippie-expand-try-functions-list
                                           'yas-hippie-try-expand))))))


(defvar el-get-my-packages
  (append
   (when (and (boundp configure-clojure-p)
              configure-clojure-p)
     '(clojure-mode
       clojure-snippets))
   (when (and (boundp configure-scheme-p)
              configure-scheme-p)
     (if (executable-find "csi")
         '(geiser)
       (error "Scheme programming (via Chicken) is configured, but I can't find the `csi' binary! Have you read the README file?")))
   (when (and (boundp configure-go-p)
              configure-go-p)
     (if (executable-find "go")
         '(go-mode
           go-company
           go-def
           go-eldoc
           go-errcheck-el
           go-flymake
           go-imports
           go-lint)
       (error "Golang programming is configured, but I can't find the `go' binary! Have you read the README file?")))
   '(ag
     avy
     ace-window
     org-mode
     org-mode-crate
     org-jira
     color-theme-zenburn
     company-mode
     dash
     diminish
     edebug-x
     el-spice
     emacs-async
     flymake-cursor
     grep+
     helm
     ; ibuffer-vc - commenting this out for a while, I believe that it
     ; is broken at the moment.
     ledger-mode
     lua-mode
     macrostep
     magit
     markdown-mode
     multiple-cursors
     paredit
     s
     smex
     toml-mode
     unbound
     wgrep)
   (mapcar 'el-get-source-name el-get-sources)
   (when on-my-machine
     ;; Load packages with Third Party
     ;; dependencies only on my machine.
     '(emacs-eclim))))

(el-get 'sync el-get-my-packages)
