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
(setq el-get-notify-type 'message)

;; Install el-get if it isn't already present
(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.github.com/dimitri/el-get/master/el-get-install.el")
    (let (el-get-master-branch
          el-get-install-skip-emacswiki-recipes)
      (goto-char (point-max))
      (eval-print-last-sexp))))

(require 'el-get-elpa)
;; Build the El-Get copy of the package.el packages if we have not
;; built it before.  Will have to look into updating later ...
(unless (file-directory-p el-get-recipe-path-elpa)
  (el-get-elpa-build-local-recipes))

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

       (when (bound-and-true-p configure-clojure-p)
         '((:name cider)
           (:name clojure-mode)
           (:name helm-cider)
           (:name clojure-snippets)
           (:name clj-refactor)

           (:name cljstyle
                  :after (progn
                           ;; Note: I don't turn cljstyle on by
                           ;; default. The reason for this is that
                           ;; rewriting the buffer on save causes me
                           ;; to lose marks in the buffer, which is
                           ;; extremely irritating. For the moment, I
                           ;; either use cljstyle on demand (M-x
                           ;; cljstyle-mode) or from the terminal

                           ;; TODO: Write a function / hook to run
                           ;; cljstyle when I enter magit, on modified
                           ;; files.

                           ;; If you wish, you can turn on cljstyle on
                           ;; each save with:
                           ;; (add-hook 'clojure-mode-hook 'turn-on-cljstyle)
                           (defun turn-on-cljstyle ()
                             "Utility function to turn on `cljstyle-mode' and auto-formatting."
                             (if (executable-find "cljstyle")
                                 (cljstyle-mode +1)
                               (message "Could not find `cljstyle' on $PATH. Please ensure you have installed it correctly.")))))

           (:name flycheck-joker)
           (:name flycheck-clj-kondo)))

       (when (bound-and-true-p configure-python-p)
         '((:name elpy :after (progn (elpy-enable)))))

       (when (bound-and-true-p configure-rust-p)
         (if (and (executable-find "rustc")
                  (executable-find "cargo"))
             '((:name rust-mode
                      :after (progn (add-to-list 'auto-mode-alist
                                                 '("\\.rs\\'" . rust-mode))
                                    (add-hook 'rust-mode-hook
                                              (lambda ()
                                                (setq indent-tabs-mode nil)))
                                    (setq rust-format-on-save t)))
               (:name cargo)

               (:name flycheck-rust)

               ;; Expects RUST_SRC_PATH env variable to be set.
               (:name emacs-racer
                      :after (progn
                               (when (not (exec-path-from-shell-getenv "RUST_SRC_PATH"))
                                 (message "Rust Source Path is not defined. Jumping to source might not work!"))
                               (add-hook 'racer-mode-hook #'eldoc-mode)
                               (add-hook 'racer-mode-hook #'company-mode))))

           (error "Rust Lang programming is configured, but I can't find the `rustc' binary! Have you read the README file?")))

       (when (bound-and-true-p configure-js-p)
         '((:name rjsx-mode
                  :after (progn (add-to-list 'auto-mode-alist '("\\.js\\'" . rjsx-mode))
                                (add-to-list 'auto-mode-alist '("\\.jsx\\'" . rjsx-mode))
                                (add-to-list 'auto-mode-alist '("\\.json\\'" . rjsx-mode))
                                (setq js2-basic-offset 2
                                      js-switch-indent-offset 2)))))

;;; Conditional Installs --- Things that depend on external services.

       ;; Format JS, JSX files on save event.
       ;; Prerequisite: npm install -g prettier
       (when (executable-find "prettier")
         '((:name prettier-js
                  :after (add-hook 'rjsx-mode-hook #'prettier-js-mode))))

;;; All the other recipes
       '((:name ace-link
                :after
                (progn (ace-link-setup-default)
                       (ace-link-setup-default (kbd "M-g o"))
                       (define-key org-mode-map (kbd "M-g o") 'ace-link-org)
                       (define-key org-agenda-mode-map (kbd "M-g o") 'ace-link-org-agenda)
                       (with-eval-after-load 'org-brain
                         (define-key org-brain-visualize-mode-map (kbd "M-g o")
                           'ace-link-org))
                       (with-eval-after-load 'ert
                         (define-key ert-results-mode-map (kbd "o")
                           'ace-link-help))))
         ;; Breaking alphabetical recipe pattern for link-hint, to
         ;; ensure it is next to ace-link. Both provide the same
         ;; functionality, but link-hint also allows for copying
         ;; links, which is very valuable to me.
         (:name link-hint
                :after (progn (global-set-key (kbd "M-g c") 'link-hint-copy-link)))

         (:name ace-window
                :after (progn (global-set-key (kbd "C-x o") 'ace-window)
                              (setq aw-scope 'frame
                                    aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))))

         (:name all-the-icons)

         (:name all-the-icons-dired
                :after (progn
                         (add-hook 'dired-mode-hook
                                   #'all-the-icons-dired-mode)))

         (:name all-the-icons-ibuffer
                :after (progn (all-the-icons-ibuffer-mode 1)))

         (:name ansible-doc
                :after (with-eval-after-load 'yaml-mode
                         (progn (add-hook 'yaml-mode-hook #'ansible-doc-mode))))

         (:name anzu
                :after (progn (global-anzu-mode +1)
                              (setq anzu-mode-lighter ""
                                    anzu-deactivate-region t
                                    anzu-search-threshold 1000
                                    anzu-replace-threshold 50
                                    anzu-replace-to-string-separator " => ")
                              (define-key isearch-mode-map
                                [remap isearch-query-replace]
                                #'anzu-isearch-query-replace)
                              (define-key isearch-mode-map
                                [remap isearch-query-replace-regexp]
                                #'anzu-isearch-query-replace-regexp)
                              (defalias 'qrr 'anzu-query-replace-regexp)))

         (:name ascii-art-to-unicode)

         (:name auctex
                :after (progn
                         (add-hook 'TeX-after-compilation-finished-functions
                                   #'TeX-revert-document-buffer)))

         (:name avy
                :after (progn (avy-setup-default)
                              (global-set-key (kbd "M-g C-j") 'avy-resume)
                              (global-set-key (kbd "M-g g") 'avy-goto-line)
                              (global-set-key (kbd "M-g w") 'avy-goto-word-1)
                              (global-set-key (kbd "M-g SPC") 'avy-goto-word-1)))

         (:name color-theme-leuven
                :after (progn (setq leuven-scale-outline-headlines nil
                                    leuven-scale-org-agenda-structure nil
                                    leuven-scale-volatile-highlight nil)))

         (:name dash-at-point
                :after (progn (global-set-key (kbd "C-c d d") 'dash-at-point)))

         (:name deadgrep
                :after (progn (global-set-key (kbd "M-g a") 'deadgrep)))

         (:name dumb-jump
                :after (progn (dumb-jump-mode)
                              (add-hook 'xref-backend-functions #'dumb-jump-xref-activate)
                              (define-key dumb-jump-mode-map (kbd "C-M-q") nil)
                              (define-key dumb-jump-mode-map (kbd "C-M-p") nil)
                              (define-key dumb-jump-mode-map (kbd "C-c d g") 'dumb-jump-go)
                              (define-key dumb-jump-mode-map (kbd "C-c d b") 'dumb-jump-back)
                              (setq dumb-jump-selector 'popup)))

         ;; Change-Inner, Expand-Region and Multiple-Cursors are
         ;; interesting selection and editing tools that go together.
         (:name change-inner
                :after (progn (global-set-key (kbd "M-i") 'change-inner)
                              (global-set-key (kbd "M-o") 'change-outer)))
         (:name expand-region
                :after (progn (global-set-key (kbd "C-=") 'er/expand-region)))
         (:name multiple-cursors
                :after (progn (global-set-key (kbd "C-c = 0")
                                              'mc/mark-next-like-this)
                              (global-set-key (kbd "C-c = -")
                                              'mc/mark-all-dwim)
                              (global-set-key (kbd "C-c = _")
                                              'mc/mark-all-symbols-like-this-in-defun)))
         ;; Breaking alphabetical order to put Change-Inner,
         ;; Expand-Region and Multiple-Cursors next to each other.

         ;; (:name es-mode
         ;;        :load-path "."
         ;;        :features ob-elasticsearch)

         (:name edit-server
                :after (progn (edit-server-start)))

         (:name eglot
                :after (with-eval-after-load 'eglot
                         (add-to-list 'eglot-server-programs
                                      '(yaml-mode . ("yaml-language-server" "--stdio")))))

         ;; Easy kill might remove the complete need of `change-inner'
         ;; and `expand-region'. I'll observe for a bit and then take
         ;; the call of whether to keep the changes in or not.
         (:name easy-kill
                :after (progn (global-set-key [remap kill-ring-save] 'easy-kill)
                              (global-set-key [remap mark-sexp] 'easy-mark)))

         (:name emacs-emojify)

         (:name fancy-battery
                :after (progn (setq fancy-battery-show-percentage t)
                              (add-hook 'after-init-hook #'fancy-battery-mode)))

         (:name forge
                :after (progn (setq auth-sources '("~/.authinfo.gpg"))
                              (with-eval-after-load 'magit
                                (require 'forge))))

         (:name flycheck
                :after (progn (setq flycheck-global-modes '(not org-mode))
                              (global-flycheck-mode)))

         (:name flycheck-inline
                :after (progn (with-eval-after-load 'flycheck
                                (add-hook 'flycheck-mode-hook
                                          #'flycheck-inline-mode))))
         (:name helm
                :after (progn (require 'init-helm)))

         (:name helm-descbinds
                :after (progn (require 'helm-descbinds)
                              (helm-descbinds-mode)))

         (:name helm-notmuch)

         (:name helm-org
                :before (progn (require 'helm-config))
                :after (progn (require 'helm-org)
                              (add-to-list 'helm-completing-read-handlers-alist
                                           '(org-capture . helm-org-completing-read-tags))
                              (add-to-list 'helm-completing-read-handlers-alist
                                           '(org-set-tags . helm-org-completing-read-tags))
                              (global-set-key (kbd "C-x c o b")
                                              'helm-org-in-buffer-headings)
                              (global-set-key (kbd "C-x c o a")
                                              'helm-org-agenda-files-headings)))

         (:name helm-projectile
                :before (progn (setq projectile-keymap-prefix (kbd "C-x c p"))))

         (:name helm-system-packages)

         (:name linkd)

         ;; Note: `powerline' needs to be installed before `moe-theme'
         (:name powerline)
         (:name moe-theme)

         (:name magit
                :after (progn
                         (global-set-key (kbd "C-x g") 'magit-status)
                         (setq magit-completing-read-function
                               #'helm--completing-read-default
                               magit-diff-refine-hunk t
                               magit-diff-refine-ignore-whitespace nil)

                         (eval-after-load 'info
                           '(progn (info-initialize)
                                   (add-to-list 'Info-directory-list
                                                (concat el-get-dir "magit/"))))))

         (:name move-text
                :after (progn (global-set-key (kbd "C-c <up>") 'move-text-up)
                              (global-set-key (kbd "C-c <down>") 'move-text-down)))

         (:name nov.el
                :after (progn (add-to-list 'auto-mode-alist
                                           '("\\.epub\\'" . nov-mode))))
         (:name ob-mermaid
                :after (progn
                         (setq ob-mermaid-cli-path
                               (expand-file-name "~/node_modules/.bin/mmdc"))))

         (:name org-board
                :after (progn (global-set-key (kbd "C-c o")
                                              org-board-keymap)))
         (:name org-brain)

         (:name org-chef)

         (:name org-cliplink)

         (:name org-noter
                :after (progn (add-hook 'org-noter-insert-heading-hook
                                        #'org-id-get-create)))

         (:name org-pomodoro
                :after (progn (setq org-pomodoro-keep-killed-pomodoro-time t
                                    org-pomodoro-clock-break t)
                              (global-set-key (kbd "C-x c P") 'org-pomodoro)))

         (:name org-superstar
                :after (progn (add-hook 'org-mode-hook
                                        (lambda () (org-superstar-mode 1)))))

         (:name org-web-tools)

         (:name ox-hugo
                :after (with-eval-after-load 'ox
                         (require 'ox-hugo)))

         (:name pcre2el)

         (:name pdf-tools
                :after (progn (pdf-tools-install)))

         (:name plantuml-mode
                :after (progn (setq plantuml-default-exec-mode 'jar)
                              (add-to-list 'auto-mode-alist
                                           '("\\.puml$" . plantuml-mode))
                              ;; Note: You need to define
                              ;; `plantuml-jar-path' to whereever the
                              ;; jar is downloaded on your system.
                              ))

         (:name poet
                :after (progn
                         ;; (set-face-attribute 'default nil
                         ;;                     :family "Iosevka"
                         ;;                     :height 130)
                         ;; (set-face-attribute 'fixed-pitch nil
                         ;;                     :family "Iosevka")
                         ;; (set-face-attribute 'variable-pitch nil
                         ;;                     :family "Baskerville")
                         ))

         (:name rainbow-mode)

         (:name restclient)

         (:name saveplace-pdf-view)

         (:name shrface
                :after (progn
                         (add-hook 'eww-after-render-hook #'shrface-mode)
                         (with-eval-after-load 'nov
                           (add-hook 'nov-mode-hook #'shrface-mode)
                           (setq nov-shr-rendering-functions
                                 (append nov-shr-rendering-functions
                                         shr-external-rendering-functions)))
                         (with-eval-after-load 'shrface
                           (shrface-basic)
                           (shrface-trial)
                           (shrface-default-keybindings)
                           (define-key shrface-mode-map
                             (kbd "M-l") 'shrface-links-helm)
                           (define-key shrface-mode-map
                             (kbd "M-h") 'shrface-headline-helm)
                           (setq shrface-href-versatile t)
                           (set-face-attribute 'variable-pitch nil
                                               :font "-PfEd-FantasqueSansMono Nerd Font-normal-normal-normal-*-26-*-*-*-m-0-iso10646-1"))))

         (:name sicp
                :after (progn
                         (with-eval-after-load 'info
                           (progn (info-initialize)
                                  (add-to-list 'Info-directory-list
                                               (concat el-get-dir "sicp/"))))))

         (:name spaceline-all-the-icons)

         (:name smart-tab
                :after (progn
                         (setq smart-tab-using-hippie-expand t
                               smart-tab-expand-eolp nil
                               smart-tab-user-provided-completion-function 'company-complete
                               smart-tab-completion-functions-alist
                               '((ledger-mode . dabbrev-completion)))
                         (global-smart-tab-mode 1)))

	     (:name emacs-tree-sitter)

         (:name ts)

         (:name unicode-fonts
                :after (progn (unicode-fonts-setup)))

         (:name writegood)

         (:name xterm-color
                :after (progn (require 'xterm-color)
                              (add-hook 'comint-preoutput-filter-functions
                                        #'xterm-color-filter)
                              (setq comint-output-filter-functions
                                    (remove 'ansi-color-process-output
                                            comint-output-filter-functions))))

         (:name yaml-mode
                :after (progn (add-hook 'yaml-mode-hook #'superword-mode)))

         (:name yaml-imenu
                :after (progn (yaml-imenu-enable)))

         (:name yasnippet
                :after (progn (yas-global-mode 1)
                              (add-to-list 'hippie-expand-try-functions-list
                                           'yas-hippie-try-expand))))))


(defvar el-get-my-packages
  (append

   (when (bound-and-true-p configure-scheme-p)
     (if (executable-find "csi")
         '(geiser)
       (error "Scheme programming (via Chicken) is configured, but I can't find the `csi' binary! Have you read the README file?")))

   (when (bound-and-true-p configure-go-p)
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
     org-mode
     org-mode-crate
     org-gcal
     org-jira
     org-tree-slide
     color-theme-zenburn
     color-theme-idea-darkula
     company-mode
     company-auctex
     dash
     diminish
     edebug-x
     el-spice
     emacs-async
     flymake-cursor
     grep+
     ;; ibuffer-vc - commenting this out for a while, I believe that it
     ;; is broken at the moment.
     ido-completing-read-plus
     jinja2-mode
     keycast
     ledger-mode
     lua-mode
     macrostep
     markdown-mode
     paredit
     paredit-cheatsheet
     ;; rst-mode
     s
     smex
     toml-mode
     unbound
     wgrep
     yasnippet-snippets)

   (mapcar 'el-get-source-name el-get-sources)

   (when on-my-linux-machine
     ;; Load packages with Third Party
     ;; dependencies only on my machine.
     '(;; eclim
       elpher))))

(el-get 'sync el-get-my-packages)

(provide 'init-el-get)
;;; init-el-get.el ends here
