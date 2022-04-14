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
  "User-configuration for various packages and user-defined recipes live here.")
(defvar el-get-my-recipes
  (concat el-get-user-package-directory "personal-recipes/")
  "User-defined recipes live here.")

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
    (let ((el-get-git-install-url "https://github.com/vedang/el-get.git")
          ;; Read through https://github.com/vedang/el-get/issues/1 for context
          (el-get-install-branch "dev")
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

(with-eval-after-load 'info
  (progn
    (info-initialize)
    (add-to-list 'Info-directory-list
                 (concat el-get-dir "el-get/"))
    (require 'scroll-other-window)
    (add-hook 'Info-mode-hook #'sow-mode)))

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
             '((:name rustic
                      :after (progn (setq rustic-lsp-client 'eglot
					                      rustic-format-on-save t))))

           (error "Rust Lang programming is configured, but you need to install the `rustc', `cargo' and `rust-analyzer' binaries! Please check the README file for installation instructions")))

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

       ;; Keep updating the Zoxide DB for all the paths I open, keep
       ;; making Zoxide more powerful.
       (when (executable-find "zoxide")
         '((:name zoxide.el
                  :after (progn
                           (add-hook 'find-file-hook #'zoxide-add)
                           (add-hook 'projectile-after-switch-project-hook
                                     #'zoxide-add)
                           (add-hook 'dired-mode-hook #'zoxide-add)))))

;;; Things to install only on my machine. These are currently not set
;;; up properly enough for public consumption, or require too many
;;; third party dependencies.
       (when on-my-linux-machine
         '((:name code-compass
                  :after (progn
                           (setq c/preferred-browser "firefox")))

           (:name elfeed)

           (:name elpher)

           (:name helm-dash
                  :after (setq dash-docs-browser-func 'eww))))
;;; All the prettiness of Emacs is captured here.
       (when prettify-my-emacs
         (append
          ;; Beautiful diffs
          ;; Prerequisite: cargo install delta
          (when (executable-find "delta")
            '((:name magit-delta
                     :after (progn
                              (defun turn-on-magit-delta ()
                                (magit-delta-mode +1))
                              (add-hook 'magit-mode-hook #'turn-on-magit-delta)))))

	      '((:name all-the-icons-dired
                   :after (progn
                            (add-hook 'dired-mode-hook
                                      #'all-the-icons-dired-mode))))))

;;; All the other recipes
       '((:name ace-link
                :after
                (progn (ace-link-setup-default)
                       (ace-link-setup-default (kbd "M-g o"))
                       (with-eval-after-load 'org
                         (define-key org-mode-map (kbd "M-g o") 'ace-link-org))
                       (with-eval-after-load 'org-agenda
                         (define-key org-agenda-mode-map (kbd "M-g o")
                           'ace-link-org-agenda))
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

         (:name all-the-icons-ibuffer
                :after (progn (all-the-icons-ibuffer-mode 1)))

         (:name ansible
                :after (with-eval-after-load 'yaml-mode
                         (defun turn-on-ansible-mode ()
                           (ansible 1))
                         (add-hook 'yaml-mode-hook #'turn-on-ansible-mode)))

         (:name ansible-doc
                :after (with-eval-after-load 'yaml-mode
                         (add-hook 'yaml-mode-hook #'ansible-doc-mode)))

         (:name ansi-color)

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

         (:name ascii-art-to-unicode
                :after (require 'ascii-art-to-unicode))

         (:name auctex
                :before (with-eval-after-load 'tex
                          (setq TeX-source-correlate-method 'synctex)
                          (TeX-source-correlate-mode 1))
                :after (progn
                         (add-hook 'TeX-after-compilation-finished-functions
                                   #'TeX-revert-document-buffer)
                         (setq TeX-engine 'luatex)))

         (:name avy
                :after (progn (avy-setup-default)
                              (global-set-key (kbd "M-g C-j") 'avy-resume)
                              (global-set-key (kbd "M-g g") 'avy-goto-line)
                              (global-set-key (kbd "M-g w") 'avy-goto-word-1)
                              (global-set-key (kbd "M-g SPC") 'avy-goto-word-1)))

         (:name bookmark+
                :after (progn (require 'bookmark+)))

         ;; (:name color-theme-leuven
         ;;        :after (progn (setq leuven-scale-outline-headlines nil
         ;;                            leuven-scale-org-agenda-structure nil
         ;;                            leuven-scale-volatile-highlight nil)))

         (:name company-mode
                :after (progn (add-hook 'after-init-hook #'global-company-mode)
                              (setq company-require-match nil
                                    company-tooltip-align-annotations t)
                              (with-eval-after-load 'company
                                (define-key company-active-map
                                  (kbd "TAB") 'company-complete))))

         (:name company-auctex)

         (:name company-ansible)

         (:name dash-at-point
                :after (progn (global-set-key (kbd "C-c d d") 'dash-at-point)))

         (:name deadgrep
                :after (progn (global-set-key (kbd "M-g a") 'deadgrep)))

         (:name diminish
                :before (progn
                          (defvar vm/diminish-modes
                            '((flyspell . flyspell-mode)
                              (paredit . paredit-mode)
                              (yasnippet . yas-minor-mode)
                              (whitespace . whitespace-mode)
                              (smart-tab . smart-tab-mode)
                              (clj-refactor . clj-refactor-mode)
                              (simple . visual-line-mode)
                              (subword . subword-mode)
                              (subword . superword-mode)
                              (helm-mode . helm-mode)
                              (projectile . projectile-mode)
                              (hideshow . hs-minor-mode)
                              (noutline . outline-minor-mode)
                              (ansible-doc . ansible-doc-mode)
                              (autorevert . auto-revert-mode)
                              (company . company-mode)
                              (gcmh . gcmh-mode))
                            "Tuples of (LIBRARY-NAME . MODE-NAME) that I don't want to see on the modeline.")
                          (defmacro vm/diminish-that-line ()
                            (cons 'progn
                                  (mapcar (lambda (dim-mode)
                                            `(with-eval-after-load (quote ,(car dim-mode)) (diminish (quote ,(cdr dim-mode)))))
                                          vm/diminish-modes))))
                :after (vm/diminish-that-line))

         (:name docker)

         (:name dockerfile-mode)

         (:name doom-themes
                :after (progn ;; Global settings (defaults)
                         (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
                               doom-themes-enable-italic t) ; if nil, italics is universally disabled
                         (load-theme 'doom-acario-dark t)

                         ;; Enable flashing mode-line on errors
                         (require 'doom-themes-ext-visual-bell)
                         (doom-themes-visual-bell-config)
                         ;; Enable custom neotree theme
                         (require 'doom-themes-ext-neotree)
                         (doom-themes-neotree-config)
                         ;; or for treemacs users
                         (require 'doom-themes-ext-treemacs)
                         (setq doom-themes-treemacs-theme "doom-atom") ; use "doom-colors" for less minimal icon theme
                         (doom-themes-treemacs-config)
                         ;; Corrects (and improves) org-mode's native fontification.
                         (require 'doom-themes-ext-org)
                         (doom-themes-org-config)))
         (:name dumb-jump
                :after (progn (dumb-jump-mode)
                              (add-hook 'xref-backend-functions #'dumb-jump-xref-activate)
                              (define-key dumb-jump-mode-map (kbd "C-M-q") nil)
                              (define-key dumb-jump-mode-map (kbd "C-M-p") nil)
                              (define-key dumb-jump-mode-map (kbd "C-c d g") 'dumb-jump-go)
                              (define-key dumb-jump-mode-map (kbd "C-c d b") 'dumb-jump-back)
                              (setq dumb-jump-selector 'helm
                                    dumb-jump-prefer-searcher 'rg)))

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
                                      '(yaml-mode . ("yaml-language-server" "--stdio")))
                         (add-to-list 'eglot-server-programs
                                      '((tex-mode context-mode texinfo-mode bibtex-mode) . ("texlab")))
                         (add-to-list 'eglot-server-programs
                                      '(lua-mode . ("lua-language-server")))
                         (add-to-list 'eglot-server-programs
                                      '(python-mode . ("jedi-language-server")))
                         (add-to-list 'eglot-server-programs
                                      '(elixir-mode . ("elixir-ls")))
                         (add-to-list 'eglot-server-programs
                                      '((js2-mode typescript-mode) . (eglot-deno "deno" "lsp")))
                         (add-to-list 'eglot-server-programs
                                      '(zig-mode . ("zls")))
                         (add-to-list 'eglot-server-programs
                                      '(ess-r-mode . ("R" "--slave" "-e" "languageserver::run()")))

                         (require 'project)

                         (defun project-find-go-module (dir)
                           (when-let ((root (locate-dominating-file dir "go.mod")))
                             (cons 'go-module root)))

                         (cl-defmethod project-root ((project (head go-module)))
                           (cdr project))

                         (add-hook 'project-find-functions #'project-find-go-module)
			 (add-hook 'go-mode-hook 'eglot-ensure)
			 (defun eglot-format-buffer-on-save ()
			   (add-hook 'before-save-hook #'eglot-format-buffer -10 t))
			 (add-hook 'go-mode-hook #'eglot-format-buffer-on-save)))

         ;; Easy kill might remove the complete need of `change-inner'
         ;; and `expand-region'. I'll observe for a bit and then take
         ;; the call of whether to keep the changes in or not.
         (:name easy-kill
                :after (progn (global-set-key [remap kill-ring-save] 'easy-kill)
                              (global-set-key [remap mark-sexp] 'easy-mark)))

         (:name fancy-battery
                :after (progn (setq fancy-battery-show-percentage t)
                              (add-hook 'after-init-hook #'fancy-battery-mode)))

         (:name forge
                :after (progn (setq auth-sources '("~/.authinfo.gpg"))
                              (with-eval-after-load 'magit
                                (require 'forge))))

         (:name flycheck
                :after (progn (setq flycheck-global-modes '(not org-mode)
                                    flycheck-emacs-lisp-load-path 'inherit)
                              (global-flycheck-mode)))

         (:name flycheck-inline
                :after (with-eval-after-load 'flycheck
                         (add-hook 'flycheck-mode-hook #'flycheck-inline-mode)))

         (:name gcmh
                :after (progn (require 'gcmh)
                              (gcmh-mode 1)))

         (:name groovy-emacs-mode)

         (:name helm
                :after (progn (require 'init-helm)))

         (:name helm-c-yasnippet
                :after (progn (setq helm-yas-space-match-any-greedy t)
                              (global-set-key (kbd "C-x c y") 'helm-yas-complete)))

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

         (:name highlight-indentation)

         (:name linkd)

         (:name lispy
                ;; Note that `lispy-mode' is installed alongside
                ;; `pardedit-mode' via the `turn-on-paredit' function,
                ;; since we already maintain a list of lispy modes
                ;; there.
                :after (progn (setq lispy-compat
                                    '(edebug cider magit-blame-mode))
                              ;; Gimme back my `back-to-indentation'
                              (with-eval-after-load 'lispy-mode
                                (define-key lispy-mode-map (kbd "M-m") nil))))

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

                         (with-eval-after-load 'info
                           (info-initialize)
                           (add-to-list 'Info-directory-list
                                        (concat el-get-dir "magit/")))))

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

         (:name org-books
                ;; My personal settings set up the capture template
                ;; mentioned in `org-books' README
                )

         (:name org-brain
                :before (progn (autoload 'helm-brain "org-brain")
                               (global-set-key (kbd "C-c v") 'helm-brain))
                :after (progn
                         ;; Explicit require, because the implicit one
                         ;; is not working for whatever reason.
                         (require 'org-brain)
                         (setq org-brain-visualize-default-choices 'all
                               org-brain-child-linebreak-sexp '0
                               org-brain-title-max-length 60
                               org-brain-include-file-entries nil
                               org-brain-file-entries-use-title nil
                               org-brain-show-resources t
                               org-brain-show-text t
                               org-brain-visualize-use-capture-templates t
                               org-brain-scan-directories-recursively t
                               org-brain-default-file-parent "index"
                               org-brain-backlink t
                               org-brain-narrow-to-entry t
                               org-brain-quit-after-goto t)
;;; Key Bindings
                         (define-key org-mode-map (kbd "C-c b")
                           'org-brain-prefix-map)
;;; Ensure that all org-mode entries have an ID.
                         ;;
                         ;; (add-hook 'before-save-hook
                         ;;           #'org-brain-ensure-ids-in-buffer)

                         (remove-hook 'before-save-hook
                                      #'org-brain-ensure-ids-in-buffer)
;;; Configuration to integrate `org-noter'
;;; with `org-brain'
                         (defun org-brain-open-org-noter (entry)
                           "Open `org-noter' on the ENTRY. If run interactively, get ENTRY from context."
                           (interactive (list (org-brain-entry-at-pt)))
                           (org-with-point-at (org-brain-entry-marker entry)
                             (org-noter)))

                         (with-eval-after-load 'org-noter
                           (define-key org-brain-visualize-mode-map
                             (kbd "C-c n") 'org-brain-open-org-noter))

                         ;; Setup `org-expiry' and define a
                         ;; `org-agenda' function to compare
                         ;; timestamps. This package is a part of
                         ;; `org-mode' contrib/lisp folder.
                         (require 'org-expiry)

                         (setq org-expiry-inactive-timestamps t)

                         (defun org-expiry-created-comp (a b)
                           "Compare `org-expiry-created-property-name' properties of A and B."
                           (let ((ta (ignore-errors
                                       (org-time-string-to-seconds
                                        (org-entry-get (get-text-property 0 'org-marker a)
                                                       org-expiry-created-property-name))))
                                 (tb (ignore-errors
                                       (org-time-string-to-seconds
                                        (org-entry-get (get-text-property 0 'org-marker b)
                                                       org-expiry-created-property-name)))))
                             (cond ((if ta (and tb (< ta tb)) tb) -1)
                                   ((if tb (and ta (< tb ta)) ta) +1))))

                         ;; Add CREATED property when adding a new
                         ;; org-brain headline entry
                         (add-hook 'org-brain-new-entry-hook
                                   #'org-expiry-insert-created)

                         ;; Finally add a function which lets us watch
                         ;; the entries chronologically
                         (defun org-brain-timeline ()
                           "List all org-brain headlines in chronological order."
                           (interactive)
                           (let ((org-agenda-files (org-brain-files))
                                 (org-agenda-cmp-user-defined #'org-expiry-created-comp)
                                 (org-agenda-sorting-strategy '(user-defined-down)))
                             (org-tags-view nil (format "+%s>\"\"" org-expiry-created-property-name))))

                         (defface aa2u-face '((t . nil))
                           "Face for aa2u box drawing characters"
                           :group 'org-brain)

                         (advice-add #'aa2u-1c :filter-return
                                     (lambda (str) (propertize str 'face 'aa2u-face)))

                         (defun aa2u-org-brain-buffer ()
                           (let ((inhibit-read-only t))
                             (make-local-variable 'face-remapping-alist)
                             (add-to-list 'face-remapping-alist
                                          '(aa2u-face . org-brain-wires))
                             (ignore-errors (aa2u (point-min) (point-max)))))

                         (with-eval-after-load 'ascii-art-to-unicode
                           (add-hook 'org-brain-after-visualize-hook #'aa2u-org-brain-buffer))

                         (defun org-brain-insert-resource-icon (link)
                           "Insert an icon, based on content of org-mode LINK."
                           (insert (format "%s "
                                           (cond ((string-prefix-p "brain:" link)
                                                  (all-the-icons-fileicon "brain"))
                                                 ((string-prefix-p "info:" link)
                                                  (all-the-icons-octicon "info"))
                                                 ((string-prefix-p "help:" link)
                                                  (all-the-icons-material "help"))
                                                 ((string-prefix-p "http" link)
                                                  (all-the-icons-icon-for-url link))
                                                 (t
                                                  (all-the-icons-icon-for-file link))))))

                         (with-eval-after-load 'all-the-icons
                           (add-hook 'org-brain-after-resource-button-functions
                                     #'org-brain-insert-resource-icon))

;;; Add a resource to and `org-brain-entry'
;;; via `org-cliplink'
                         (with-eval-after-load 'org-cliplink
                           (defun org-brain-cliplink-resource ()
                             "Add a URL from the clipboard as an org-brain resource.
Suggest the URL title as a description for resource."
                             (interactive)
                             (let ((url (org-cliplink-clipboard-content)))
                               (org-brain-add-resource
                                url
                                (org-cliplink-retrieve-title-synchronously url)
                                t)))

                           (define-key org-brain-visualize-mode-map (kbd "L")
                             'org-brain-cliplink-resource))

                         (defun org-brain-random-reading ()
                           "Find something I've already read, to re-read."
                           (interactive)
                           (org-brain-visualize-random
                            (org-brain-headline-entries-in-file
                             (expand-file-name "linklog.org" org-brain-path))))

                         (define-key org-brain-visualize-mode-map (kbd "R")
                           'org-brain-random-reading)

                         (setq savehist-additional-variables
                               '(org-brain-headline-cache))))

         (:name org-chef)

         (:name org-cliplink
                :after (require 'org-cliplink))

         (:name orgit
                :after (setq orgit-log-save-arguments t))

         (:name orgit-forge)

         (:name org-mode)

         (:name org-contrib)

         (:name org-menu
                :after (progn
                         (with-eval-after-load 'org
                           (define-key org-mode-map (kbd "C-c m") 'org-menu))))
         (:name org-mode-crate
                :after (progn (global-set-key (kbd "C-c a") #'org-agenda)
                              (message "Press `C-c a' to get started with your agenda...")

                              (with-eval-after-load 'org
                                (require 'org-mode-crate)
                                (when on-my-machine
                                  (require 'org-crate-config)))))

         (:name org-noter
                :after (progn (add-hook 'org-noter-insert-heading-hook
                                        #'org-id-get-create)))

         (:name org-protocol-capture-html
                :after (progn (require 'org-protocol)
                              (require 'org-protocol-capture-html)))

         (:name org-pomodoro
                :after (progn (setq org-pomodoro-keep-killed-pomodoro-time t
                                    org-pomodoro-clock-break t)
                              (global-set-key (kbd "C-x c o p")
                                              #'org-pomodoro)))

         (:name org-superstar
                :after (progn (add-hook 'org-mode-hook
                                        (lambda () (org-superstar-mode 1)))))

         (:name org-transclusion
                :after (progn (global-set-key (kbd "C-c C-n a")
                                              #'org-transclusion-add)
                              (global-set-key (kbd "C-c C-n t")
                                              #'org-transclusion-mode)))

         (:name org-tree-slide
                :after (progn
                         (with-eval-after-load 'org
                           (define-key org-mode-map (kbd "<f8>")
                             'org-tree-slide-mode))
			 (require 'org-tree-slide)
                         (define-key org-tree-slide-mode-map (kbd "<f7>")
                           'org-tree-slide-move-previous-tree)
                         (define-key org-tree-slide-mode-map (kbd "<f9>")
                                     'org-tree-slide-move-next-tree)
                         (define-key org-tree-slide-mode-map (kbd "<f6>")
                                     'org-tree-slide-content)
                         (setq org-tree-slide-header t
                               org-tree-slide-slide-in-blank-lines 10)))

         (:name org-web-tools)

         (:name ox-hugo
                :after (with-eval-after-load 'ox
                         (require 'ox-hugo)))

         (:name parinfer-rust-mode
                :after (progn (setq parinfer-rust-auto-download t
                                    ;; Don't check every time,
                                    ;; just enable the damn thing.
                                    parinfer-rust-check-before-enable nil)))

         (:name package-lint)

         (:name pcre2el)


         (:name pdf-tools
                :after (progn (pdf-tools-install)
                              (setq pdf-view-use-scaling t
                                    pdf-view-use-imagemagick nil)
                              (require 'scroll-other-window)
                              (add-hook 'pdf-view-mode-hook #'sow-mode)
                              (with-eval-after-load 'org-noter
                                (add-hook 'org-noter-notes-mode-hook #'sow-mode)
                                (setq org-noter--inhibit-location-change-handler t))))

         (:name pinboard
                :after (with-eval-after-load 'org
                         (defun org-pinboard-store-link ()
                           "Store a link taken from a pinboard buffer."
                           (when (eq major-mode 'pinboard-mode)
                             (pinboard-with-current-pin pin
                                                        (org-store-link-props
                                                         :type "pinboard"
                                                         :link (alist-get 'href pin)
                                                         :description (alist-get 'description pin)))))

                         (org-link-set-parameters "pinboard"
                                                  :follow #'browse-url
                                                  :store #'org-pinboard-store-link)))
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

         (:name prettify-utils
                :after (progn
                         (defun prog-mode-prettify-symbols ()
                           "Pretty symbols in all programming modes."
                           (setq prettify-symbols-alist
                                 (prettify-utils-generate
                                  ("lambda " "λ")
                                  ("<="      "≤")
                                  (">="      "≥")
                                  ("->"      "→ ")
                                  ("=>"      "⇒")
                                  ("->>"     "⇒  ")
                                  ("fn "     "ƒ ")
                                  ("#("      "λ(")
                                  ("#{"      "∈{")))
                           (prettify-symbols-mode 1))

                         (add-hook 'prog-mode-hook #'prog-mode-prettify-symbols)

                         (defun org-mode-prettify-symbols ()
                           ;; ("#+begin_src" . "")
                           ;; ("#+title: " . "")
                           (setq prettify-symbols-alist
                                 (append (mapcan (lambda (x) (list x (cons (upcase (car x)) (cdr x))))
                                                 '(("#+begin_src" . ?)
                                                   ("#+end_src" . ?)
                                                   ("#+begin_example" . ?)
                                                   ("#+end_example" . ?)
                                                   ("#+header:" . ?)
                                                   ("#+name:" . ?﮸)
                                                   ("#+results:" . ?)
                                                   ("#+call:" . ?)
                                                   ("#+startup:" . ?)
                                                   ("#+filetags:" . ?)
		                                           ("#+html_head:" . ?)
		                                           ("#+subtitle:" . ?)
		                                           ("#+author:" . ?)
                                                   (":properties:" . ?)
                                                   (":logbook:" . ?)
                                                   (":clock:" . ?)
                                                   (":end:" . ?―)
                                                   (":resources:" . ?)))
                                         '(("TODO" . ?)
	                                       ("WAITING" . ?)
   		                                   ("WORKING" . ?)
                                           ("CANCELLED" . ?)
                                           ("MEETING" . ?)
		                                   ("DONE" . ?)
		                                   ("[#A]" . ?)
		                                   ("[#B]" . ?)
 		                                   ("[#C]" . ?)
		                                   ("[ ]" . ?)
		                                   ("[X]" . ?)
		                                   ("[-]" . ?)
                                           ("CLOCK:" . ?)
                                           (":Effort:" . ?)
		                                   ("SCHEDULED:" . ?)
		                                   ("DEADLINE:" . ?))))
                           (prettify-symbols-mode 1))

                         (add-hook 'org-mode-hook
                                   #'org-mode-prettify-symbols)
                         (add-hook 'org-agenda-mode-hook
                                   #'org-mode-prettify-symbols)))

         (:name propcheck)

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
                                               :font "FantasqueSansMono NF 22"))))

         (:name sicp
                :after (progn
                         (with-eval-after-load 'info
                           (progn (info-initialize)
                                  (add-to-list 'Info-directory-list
                                               (concat el-get-dir "sicp/"))))))

         (:name smart-tab
                :after (progn
                         (setq smart-tab-using-hippie-expand t
                               smart-tab-expand-eolp nil
                               smart-tab-user-provided-completion-function 'company-complete
                               smart-tab-completion-functions-alist
                               '((ledger-mode . dabbrev-completion)))
                         (global-smart-tab-mode 1)))

         (:name solaire-mode
                :after (solaire-global-mode +1))

         (:name spaceline-all-the-icons)

         (:name tagedit
                :after (with-eval-after-load 'sgml-mode
                         (require 'tagedit)
                         (defun turn-on-tagedit ()
                           (tagedit-add-paredit-like-keybindings)
                           (tagedit-add-experimental-features)
                           (tagedit-mode +1))
                         (add-hook 'html-mode-hook #'turn-on-tagedit)))

         (:name emacs-tree-sitter
                :after (progn (require 'tree-sitter)
                              (require 'tree-sitter-hl)
                              (require 'tree-sitter-langs)
                              (require 'tree-sitter-debug)
                              (require 'tree-sitter-query)))

         (:name ts
                :after (progn (require 'ts)))

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
                :after (progn
                         (add-hook 'yaml-mode-hook #'superword-mode)

                         ;; From https://gist.github.com/antonj/874106
                         (defun aj-toggle-fold ()
                           "Toggle fold all lines larger than indentation on current line"
                           (interactive)
                           (let ((col 1))
                             (save-excursion
                               (back-to-indentation)
                               (setq col (+ 1 (current-column)))
                               (set-selective-display
                                (if selective-display nil (or col 1))))))

                         ;; From https://github.com/yoshiki/yaml-mode/issues/25
                         (defun yaml-outline-minor-mode ()
                           (outline-minor-mode)
                           (setq outline-regexp
                                 (rx
                                  (seq
                                   bol
                                   (group (zero-or-more "  ")
                                          (or (group
                                               (seq (or (seq "\"" (*? (not (in "\"" "\n"))) "\"")
                                                        (seq "'" (*? (not (in "'" "\n"))) "'")
                                                        (*? (not (in ":" "\n"))))
                                                    ":"
                                                    (?? (seq
                                                         (*? " ")
                                                         (or (seq "&" (one-or-more nonl))
                                                             (seq ">-")
                                                             (seq "|"))
                                                         eol))))
                                              (group (seq
                                                      "- "
                                                      (+ (not (in ":" "\n")))
                                                      ":"
                                                      (+ nonl)
                                                      eol))))))))
                         (add-hook 'yaml-mode-hook #'yaml-outline-minor-mode)
                         (with-eval-after-load 'yaml-mode
                           (define-key yaml-mode-map
                             (kbd "RET") #'newline-and-indent)
                           ;; This weird key-binding to co-exist with outline-minor mode
                           (define-key yaml-mode-map
                             (kbd "C-c @ C-j") #'aj-toggle-fold))))

         (:name yaml-imenu
                :after (progn (yaml-imenu-enable)))

         (:name yasnippet
                :after (progn (yas-global-mode 1)
                              (add-to-list 'hippie-expand-try-functions-list
                                           'yas-hippie-try-expand)))

         (:name yatemplate
                :after (progn (auto-insert-mode +1))))))


(defvar el-get-my-packages
  (append

   (when (bound-and-true-p configure-scheme-p)
     (if (executable-find "csi")
         '(geiser)
       (error "Scheme programming (via Chicken) is configured, but I can't find the `csi' binary! Have you read the README file?")))

   (when (bound-and-true-p configure-go-p)
     (if (executable-find "go")
         '(go-mode)
       (error "Golang programming is configured, but I can't find the `go' binary! Have you read the README file?")))

   '(ag
     org-gcal
     org-jira
     ;; color-theme-zenburn
     ;; color-theme-idea-darkula
     dash
     edebug-x
     el-spice
     emacs-async
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

   (mapcar 'el-get-source-name el-get-sources)))

(el-get 'sync el-get-my-packages)

(provide 'init-el-get)
;;; init-el-get.el ends here
