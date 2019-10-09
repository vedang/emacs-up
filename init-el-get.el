;;; init-el-get.el --- El-get for Great Good.
;;; Author: Vedang Manerikar
;;; Created on: 22 Dec 2013
;;; Copyright (c) 2013 Vedang Manerikar <vedang.manerikar@gmail.com>
;;; Commentary:
;; This file is not part of GNU Emacs.

;;; License:

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the Do What The Fuck You Want to
;; Public License, Version 2, which is included with this distribution.
;; See the file LICENSE.txt

;;; Code:

(defvar el-get-dir
  (concat dotfiles-dirname "el-get/")
  "The sub-directory where el-get packages are installed.")
(defvar el-get-user-package-directory
  (concat dotfiles-dirname "el-get-init-files/")
  "The sub-directory where optional user-configuration for various packages, and user-defined recipes live.")
(defvar el-get-my-recipes
  (concat el-get-user-package-directory "personal-recipes/")
  "The sub-directory where user-defined recipes live, if the user needs to define and install his/her own recipes.")

;; Make the el-get directories if required
(make-directory el-get-dir t)
(make-directory el-get-my-recipes t)

;; Add el-get to the load-path. From this point onward, we're plugged
;; into the el-get package management system.
(add-to-list 'load-path (concat el-get-dir "el-get"))

;; Install el-get if it isn't already present
(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.github.com/dimitri/el-get/master/el-get-install.el")
    (let (el-get-master-branch
          el-get-install-skip-emacswiki-recipes)
      (goto-char (point-max))
      (eval-print-last-sexp))))

;; Add our personal recipes to el-get's recipe path
(add-to-list 'el-get-recipe-path el-get-my-recipes)

;;; This is the order in which the packages are loaded. Changing this
;;; order can sometimes lead to nasty surprises, especially when you
;;; are overshadowing some in-built libraries. *cough*org-mode*cough*
(when (memq window-system '(mac ns x))
  (el-get 'sync '(exec-path-from-shell)))

;; Tie volatile stuff down, so that configuration does not break.
;; Add configuration for recipes that need very minor configuration.
(setq el-get-sources
      (append
       (when (and (boundp configure-clojure-p)
                  configure-clojure-p)
         '((:name cider
                  ;; :checkout "v0.15.1"
                  )
           (:name clj-refactor
                  ;; :checkout "2.3.1"
                  )
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
                                                 '("\\.rs\\'" . rust-mode))))
               (:name flycheck-rust))
           (error "Rust Lang programming is configured, but I can't find the `rustc' binary! Have you read the README file?")))
       '((:name dash-at-point
                :after (progn (global-set-key (kbd "C-c d d") 'dash-at-point)))

         ;; Change-Inner, Expand-Region and Multiple-Cursors are
         ;; interesting selection and editing tools that go together.
         (:name change-inner
                :after (progn (global-set-key (kbd "M-i") 'change-inner)
                              (global-set-key (kbd "M-o") 'change-outer)))
         (:name edit-server
                :after (progn (edit-server-start)))
         (:name expand-region
                :after (progn (global-set-key (kbd "C-c = =")
                                              'er/expand-region)))
         (:name multiple-cursors
                :after (progn (global-set-key (kbd "C-c = 0")
                                              'mc/mark-next-like-this)
                              (global-set-key (kbd "C-c = -")
                                              'mc/mark-all-dwim)
                              (global-set-key (kbd "C-c = _")
                                              'mc/mark-all-symbols-like-this-in-defun)))

         (:name flycheck
                :after (progn (setq flycheck-global-modes
                                    '(not org-mode))
                              (global-flycheck-mode 1)))
         (:name flycheck-pos-tip
                :after (progn (eval-after-load 'flycheck
                                '(progn (require 'flycheck-pos-tip)
                                        (setq flycheck-display-errors-function
                                              #'flycheck-pos-tip-error-messages)))))

         (:name helm-projectile
                :before (progn (setq projectile-keymap-prefix (kbd "C-x c p"))))

         (:name ob-mermaid
                :after (progn
                         (setq ob-mermaid-cli-path
                               (expand-file-name "~/node_modules/.bin/mmdc"))))

         (:name org-pomodoro
                :after (progn (setq org-pomodoro-keep-killed-pomodoro-time t
                                    org-pomodoro-clock-break t)
                              (global-set-key (kbd "C-c C-x c p")
                                              'org-pomodoro)))

         (:name org-board
                :after (progn (global-set-key (kbd "C-c o")
                                              org-board-keymap)))
         (:name ox-hugo
                :after (with-eval-after-load 'ox
                         (require 'ox-hugo)))

         (:name sicp
                :after (progn
                         (eval-after-load 'info
                           '(progn (info-initialize)
                                   (add-to-list 'Info-directory-list
                                                (concat el-get-dir "sicp/"))))))
         (:name smart-tab
                :after (progn (setq smart-tab-using-hippie-expand t
                                    smart-tab-expand-eolp nil
                                    smart-tab-user-provided-completion-function 'company-complete
                                    smart-tab-completion-functions-alist '((ledger-mode . dabbrev-completion)))
                              (global-smart-tab-mode 1)))
         (:name unicode-fonts
                :after (progn (unicode-fonts-setup)))
         (:name writegood
                :after (progn (global-set-key (kbd "C-c g") 'writegood-mode)))
         (:name xterm-color
                :after (progn (require 'xterm-color)
                              (add-hook 'comint-preoutput-filter-functions 'xterm-color-filter)
                              (setq comint-output-filter-functions (remove 'ansi-color-process-output comint-output-filter-functions))))
         (:name yasnippet
                :after (progn (yas-global-mode 1)
                              (add-to-list 'hippie-expand-try-functions-list
                                           'yas-hippie-try-expand))))))


(defvar el-get-my-packages
  (append
   (when (and (boundp configure-clojure-p)
              configure-clojure-p)
     '(clojure-mode
       helm-cider
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
     org-tree-slide
     color-theme-zenburn
     color-theme-idea-darkula
     color-theme-leuven
     poet
     company-mode
     dash
     diminish
     dumb-jump
     edebug-x
     el-spice
     emacs-async
     es-mode
     flymake-cursor
     grep+
     helm
     helm-ag
     ;; ibuffer-vc - commenting this out for a while, I believe that it
     ;; is broken at the moment.
     ido-completing-read-plus
     jinja2-mode
     ledger-mode
     lua-mode
     macrostep
     magit
     markdown-mode
     paredit
     paredit-cheatsheet
     rst-mode
     s
     smex
     toml-mode
     unbound
     wgrep
     yaml-mode
     yasnippet-snippets)
   (mapcar 'el-get-source-name el-get-sources)
   (when on-my-machine
     ;; Load packages with Third Party
     ;; dependencies only on my machine.
     '(eclim))))

(el-get 'sync el-get-my-packages)
