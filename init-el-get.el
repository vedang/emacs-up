;;; init-el-get.el --- El-get for Great Good. -*- lexical-binding: t -*-
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
          ;; Read through https://github.com/vedang/el-get/issues/2 for context
          (el-get-install-branch "master")
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

(defun my/chatgpt-api-key ()
  "Return my chatgpt-api-key from auth-sources."
  (auth-source-pick-first-password :host "api.openai.com"))

;;; This is the order in which the packages are loaded. Changing this
;;; order can sometimes lead to nasty surprises, especially when you
;;; are overshadowing some in-built libraries. *cough*org-mode*cough*
(when (memq window-system '(mac ns x))
  (el-get 'sync '(exec-path-from-shell org-mode org-contrib)))

;; Tie volatile stuff down, so that configuration does not break.
;; Add configuration for recipes that need very minor configuration.
(setq el-get-sources
      (append

       ;; Clojure Config
       '((:name cider)
         (:name cider-storm
                :after (progn
                         (with-eval-after-load 'cider
                           (require 'cider-storm)
                           (define-key cider-mode-map
                                       (kbd "C-c C-f") #'cider-storm-map)
                           (define-key cider-repl-mode-map
                                       (kbd "C-c C-f") #'cider-storm-map))))
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

         (:name flycheck-clj-kondo)

         (:name jet))

;;; Conditional Installs --- Things that depend on external services.

       ;; Keep updating the Zoxide DB for all the paths I open, keep
       ;; making Zoxide more powerful.
       (when (executable-find "zoxide")
         '((:name zoxide.el
                  :after (progn
                           (add-hook 'find-file-hook #'zoxide-add)
                           (add-hook 'dired-mode-hook #'zoxide-add)))))

;;; Things to install only on my machine. These are currently not set
;;; up properly enough for public consumption, or require too many
;;; third party dependencies.
       (when on-my-linux-machine
         '((:name code-compass
                  :after (progn
                           (setq c/preferred-browser "firefox")))

           (:name helm-dash
                  :after (setq dash-docs-browser-func 'eww))))

;;; All the other recipes
       '((:name ace-link
                :after
                (progn (ace-link-setup-default)
                       (ace-link-setup-default (kbd "M-g o"))
                       (with-eval-after-load 'org
                         (define-key org-mode-map (kbd "M-g o") #'ace-link-org))
                       (with-eval-after-load 'org-agenda
                         (define-key org-agenda-mode-map (kbd "M-g o")
                                     #'ace-link-org-agenda))
                       (with-eval-after-load 'org-brain
                         (define-key org-brain-visualize-mode-map (kbd "M-g o")
                                     #'ace-link-org))
                       (with-eval-after-load 'ert
                         (define-key ert-results-mode-map (kbd "o")
                                     #'ace-link-help))))
         ;; Breaking alphabetical recipe pattern for link-hint, to
         ;; ensure it is next to ace-link. Both provide the same
         ;; functionality, but link-hint also allows for copying
         ;; links, which is very valuable to me.
         (:name link-hint
                :after (progn (global-set-key (kbd "M-g c")
                                              #'link-hint-copy-link)
                              (global-set-key (kbd "M-g O")
                                              #'link-hint-open-link)))

         (:name ace-window
                :after (progn (global-set-key (kbd "C-x o") #'ace-window)
                              (setq aw-scope 'frame
                                    aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))))

         ;; (:name ansible
         ;;        :after (with-eval-after-load 'yaml-mode
         ;;                 (defun turn-on-ansible-mode ()
         ;;                   (ansible 1))
         ;;                 (add-hook 'yaml-mode-hook #'turn-on-ansible-mode)))

         ;; (:name ansible-doc
         ;;        :after (with-eval-after-load 'yaml-mode
         ;;                 (add-hook 'yaml-mode-hook #'ansible-doc-mode)))

         (:name ansi-color)

         ;; (:name anzu
         ;;        :after (progn (global-anzu-mode +1)
         ;;                      (setq anzu-mode-lighter ""
         ;;                            anzu-deactivate-region t
         ;;                            anzu-search-threshold 1000
         ;;                            anzu-replace-threshold 50
         ;;                            anzu-replace-to-string-separator " => ")
         ;;                      (define-key isearch-mode-map
         ;;                                  [remap isearch-query-replace]
         ;;                                  #'anzu-isearch-query-replace)
         ;;                      (define-key isearch-mode-map
         ;;                                  [remap isearch-query-replace-regexp]
         ;;                                  #'anzu-isearch-query-replace-regexp)
         ;;                      (defalias 'qrr 'anzu-query-replace-regexp)))

         (:name apheleia
                :after (progn
                         (apheleia-global-mode +1)
                         (with-eval-after-load 'apheleia-formatters
                           (push '(zprint . ("zprint")) apheleia-formatters))))

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
                              (global-set-key (kbd "M-g C-j") #'avy-resume)
                              (global-set-key (kbd "M-g g") #'avy-goto-line)
                              (global-set-key (kbd "M-g w") #'avy-goto-word-1)
                              (global-set-key (kbd "M-g SPC") #'avy-goto-word-1)))
         (:name bnf-mode)

         (:name bookmark+
                :after (progn (require 'bookmark+)))

         (:name calfw
                :after (with-eval-after-load 'org
                         (require 'calfw)
                         (require 'calfw-org)
                         (setq cfw:org-agenda-schedule-args
                               '(:deadline :scheduled :timestamp))))

         (:name calfw-blocks
                :after (with-eval-after-load 'org
                         (require 'calfw-blocks)

                         ;; From the readme of calfw-blocks
                         (defun cfw:open-calendar-agenda-blocks ()
                           (interactive)
                           (cfw:open-calendar-buffer
                            :contents-sources
                            (list
                             (cfw:org-create-source "medium purple"))
                            :view 'block-day))

                         (setq calfw-blocks-earliest-visible-time '(6 0)
                               calfw-blocks-show-time-grid t
                               calfw-blocks-show-current-time-indicator t
                               calfw-blocks-default-event-length 1
                               cfw:highlight-today nil)))

         (:name chatgpt-shell
                :after (progn
                         (setq chatgpt-shell-openai-key #'my/chatgpt-api-key)
                         (setq dall-e-shell-openai-key #'my/chatgpt-api-key)))

         (:name combobulate
                :after (with-eval-after-load 'combobulate
                         (add-hook 'css-ts-mode-hook #'combobulate-mode)
                         (add-hook 'html-ts-mode-hook #'combobulate-mode)
                         (add-hook 'js-ts-mode-hook #'combobulate-mode)
                         (add-hook 'json-ts-mode-hook #'combobulate-mode)
                         (add-hook 'python-ts-mode-hook #'combobulate-mode)
                         (add-hook 'tsx-ts-mode-hook #'combobulate-mode)
                         (add-hook 'typescript-ts-mode-hook #'combobulate-mode)
                         (add-hook 'yaml-ts-mode-hook #'combobulate-mode)))

         (:name company-mode
                :after (progn (add-hook 'after-init-hook #'global-company-mode)
                              (setq company-require-match nil
                                    company-tooltip-align-annotations t
                                    company-idle-delay 0.75)
                              (with-eval-after-load 'company
                                (define-key company-active-map
                                            (kbd "TAB") 'company-complete))))

         (:name company-auctex)

         (:name company-ansible)

         (:name dart-mode)

         (:name denote
                :after (progn
                         (setq denote-directory
                               (expand-file-name "~/Tresors/Documents/diary/notes")
                               denote-date-prompt-use-org-read-date t
                               denote-prompts '(title keywords subdirectory signature)
                               denote-backlinks-show-context t
                               denote-excluded-directories-regexp "data"
                               denote-org-front-matter
                               ":PROPERTIES:
:ID: %4$s
:CREATED: %2$s
:END:
#+title:      %1$s
#+filetags:   %3$s
#+date:       %2$s
#+identifier: %4$s
\n")
                         (require 'denote-silo-extras)
                         (require 'denote-journal-extras)
                         (require 'denote-org-extras)

                         (add-to-list 'denote-silo-extras-directories
                                      (expand-file-name "~/src/prototypes/salher-docs"))

                         (setq denote-dired-directories-include-subdirectories t
                               denote-dired-directories denote-silo-extras-directories)

                         (add-hook 'text-mode-hook
                                   #'denote-fontify-links-mode-maybe)
                         (add-hook 'dired-mode-hook
                                   #'denote-dired-mode-in-directories)

                         (denote-rename-buffer-mode 1)
                         (setq denote-rename-buffer-format "[D] %s %t%b")

                         ;; Journal settings
                         (setq denote-journal-extras-keyword "")

                         ;; I use Yasnippet to expand these into a
                         ;; better template.
                         (add-to-list 'denote-templates
                                      '(reference-note . "reference"))
                         (add-to-list 'denote-templates
                                      '(morning . "morningpage"))
                         (add-to-list 'denote-templates
                                      '(emotion . "emotion"))
                         (add-to-list 'denote-templates
                                      '(insight . "insight"))
                         (add-to-list 'denote-templates
                                      '(weekly_intentions . "weekint"))
                         (add-to-list 'denote-templates
                                      '(weekly_report . "weekrpt"))
                         (add-to-list 'denote-templates
                                      '(checkin . "checkin"))

                         ;;; Key Bindings
                         ;; Creating New Notes
                         (global-set-key (kbd "C-c d n") #'denote-create-note)
                         (global-set-key (kbd "C-c d N")
                                         #'denote-silo-extras-select-silo-then-command)
                         ;; Opening Notes
                         (global-set-key (kbd "C-c d o")
                                         #'denote-open-or-create)
                         (global-set-key (kbd "C-c d O")
                                         #'denote-silo-extras-open-or-create)

                         ;; Linking Notes
                         (global-set-key (kbd "C-c d l")
                                         #'denote-link-or-create)
                         (global-set-key (kbd "C-c d L")
                                         #'denote-link-after-creating-with-command)

                         ;; Links and Backlinks
                         (global-set-key (kbd "C-c d B") #'denote-backlinks)
                         (global-set-key (kbd "C-c d b") #'denote-find-backlink)
                         (global-set-key (kbd "C-c d f") #'denote-find-link)
                         (global-set-key (kbd "C-c d h") #'denote-org-extras-link-to-heading)

                         ;; Journal Entries
                         (global-set-key (kbd "C-c d j")
                                         #'denote-journal-extras-new-entry)
                         ;; Publishing writing
                         (defun denote-publishing-extras-new-microblog-entry (&optional date)
                           "Create a new microblog entry.
Set the title of the new entry according to the value of the user option
`denote-journal-extras-title-format'.

With optional DATE as a prefix argument, prompt for a date.  If
`denote-date-prompt-use-org-read-date' is non-nil, use the Org
date selection module.

When called from Lisp DATE is a string and has the same format as
that covered in the documentation of the `denote' function.  It
is internally processed by `denote-parse-date'."
                           (interactive (list (when current-prefix-arg (denote-date-prompt))))
                           (let ((internal-date (denote-parse-date date))
                                 (denote-directory (file-name-as-directory (expand-file-name "published" denote-directory))))
                             (denote
                              (denote-journal-extras-daily--title-format internal-date)
                              '("draft" "microblog")
                              nil nil date
                              ;; See YASnippet
                              "microblog")))

                         (defun denote-publishing-extras-new-blog-entry (&optional date)
                           "Create a new blog entry.

With optional DATE as a prefix argument, prompt for a date.  If
`denote-date-prompt-use-org-read-date' is non-nil, use the Org
date selection module.

When called from Lisp DATE is a string and has the same format as
that covered in the documentation of the `denote' function.  It
is internally processed by `denote-parse-date'."
                           (interactive (list (when current-prefix-arg (denote-date-prompt))))
                           (let ((internal-date (denote-parse-date date))
                                 (denote-directory (file-name-as-directory (expand-file-name "published" denote-directory))))
                             (denote
                              (denote-title-prompt)
                              '("draft")
                              nil nil date
                              ;; See YASnippet
                              "fullblog")))

                         (global-set-key (kbd "C-c d p m")
                                         #'denote-publishing-extras-new-microblog-entry)
                         (global-set-key (kbd "C-c d p b")
                                         #'denote-publishing-extras-new-blog-entry)

                         ;; Renaming Files
                         (global-set-key (kbd "C-c d r") #'denote-rename-file)
                         (global-set-key (kbd "C-c d R")
                                         #'denote-rename-file-using-front-matter)
                         (global-set-key (kbd "C-c d a")
                                         #'denote-add-front-matter)
                         (global-set-key (kbd "C-c d k") #'denote-rename-file-keywords)
                         (let ((map dired-mode-map))
                           (define-key map (kbd "C-c C-d i")
                                       #'denote-link-dired-marked-notes)
                           (define-key map (kbd "C-c C-d r")
                                       #'denote-dired-rename-marked-files)
                           (define-key map (kbd "C-c C-d R")
                                       #'denote-dired-rename-marked-files-with-keywords)
                           (define-key map (kbd "C-c C-d A")
                                       #'denote-dired-rename-marked-files-add-keywords)
                           (define-key map (kbd "C-c C-d K")
                                       #'denote-dired-rename-marked-files-remove-keywords))

                         ;; More functionality
                         (setq denote-org-store-link-to-heading t
                               denote-rename-confirmations nil
                               denote-save-buffers t)

                         (defun denote-org-extras-extract-org-subtree-silo (silo)
                           "Create new Denote note using current Org subtree.

Select SILO, a file path from `denote-silo-extra-directories',
and then call `denote-org-extras-extract-org-subtree'."
                           (interactive
                            (list (completing-read "Select a silo: "
                                                   denote-silo-extras-directories nil t)))
                           (let ((denote-directory silo))
                             (denote-org-extras-extract-org-subtree)))

                         (defun denote-subdirectory-and-signature ()
                           "Create note while prompting for a file signature and subdirectory.

This is the equivalent to calling `denote' when `denote-prompts'
is set to \\='(signature subdirectory title keywords)."
                           (declare (interactive-only t))
                           (interactive)
                           (let ((denote-prompts '(signature subdirectory title keywords)))
                             (call-interactively #'denote)))

                         (add-to-list 'denote-commands-for-new-notes
                                      'denote-subdirectory-and-signature)

                         (defun my-denote-link-ol-get-id ()
                           "Get the CUSTOM_ID of the current entry.
If the entry already has a CUSTOM_ID, return it as-is, else
create a new one."
                           (interactive)
                           (let* ((pos (point))
                                  (id (org-entry-get pos "CUSTOM_ID")))
                             (if (and (stringp id) (string-match-p "\\S-" id))
                                 id
                               (setq id (org-id-new "h"))
                               (org-entry-put pos "CUSTOM_ID" id)
                               id)))

                         (defun my-denote-link-global (file file-type description &optional id-only)
                           "Like the `denote-link', but works in any buffer.
The FILE, FILE-TYPE, DESCRIPTION, and ID-ONLY have the same meaning as
in `denote-link'."
                           (interactive
                            (let* ((file (denote-file-prompt nil "Link to FILE"))
                                   (file-type (denote-filetype-heuristics buffer-file-name))
                                   (description (when (file-exists-p file)
                                                  (denote--link-get-description file))))
                              (list file file-type description current-prefix-arg)))
                           (unless (file-exists-p file)
                             (user-error "The linked file does not exists"))
                           (let ((beg (point)))
                             (denote--delete-active-region-content)
                             (insert (denote-format-link file description file-type id-only))
                             (unless (derived-mode-p 'org-mode)
                               (make-button beg (point) 'type 'denote-link-button))))

                         ;; Opening Dired on a Search
                         (global-set-key (kbd "C-c d s") #'denote-sort-dired)

                         (defun my-denote--split-luhman-sig (signature)
                           "Split numbers and letters in Luhmann-style SIGNATURE string."
                           (replace-regexp-in-string
                            "\\([a-zA-Z]+?\\)\\([0-9]\\)" "\\1=\\2"
                            (replace-regexp-in-string
                             "\\([0-9]+?\\)\\([a-zA-Z]\\)" "\\1=\\2"
                             signature)))

                         (defun my-denote--pad-sig (signature)
                           "Create a new signature with padded spaces for all components"
                           (combine-and-quote-strings
                            (mapcar
                             (lambda (x)
                               (string-pad x 5 32 t))
                             (split-string (my-denote--split-luhman-sig signature) "=" t))
                            "="))

                         (defun my-denote-sort-for-signatures (sig1 sig2)
                           "Return non-nil if SIG1 is smaller that SIG2.
Perform the comparison with `string<'."
                           (string< (my-denote--pad-sig sig1) (my-denote--pad-sig sig2)))

                         ;; Change the sorting function only when we sort by signature.
                         (setq denote-sort-signature-comparison-function
                               #'my-denote-sort-for-signatures)))

         (:name denote-explore)

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
                              (hideshow . hs-minor-mode)
                              (noutline . outline-minor-mode)
                              (ansible-doc . ansible-doc-mode)
                              (autorevert . auto-revert-mode)
                              (company . company-mode)
                              (gcmh . gcmh-mode)
                              (org-remark . org-remark-mode)
                              (org-remark-global-tracking . org-remark-global-tracking-mode)
                              (org-pomodoro-third-time . org-pomodoro-third-time-mode)
                              (apheleia-core . apheleia-mode))
                            "Tuples of (LIBRARY-NAME . MODE-NAME) that I don't want to see on the modeline.")
                          (defmacro vm/diminish-that-line ()
                            (cons 'progn
                                  (mapcar (lambda (dim-mode)
                                            `(with-eval-after-load (quote ,(car dim-mode)) (diminish (quote ,(cdr dim-mode)))))
                                          vm/diminish-modes))))
                :after (vm/diminish-that-line))

         (:name docker)

         (:name doom-themes
                :after (progn ;; Global settings (defaults)
                         (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
                               doom-themes-enable-italic t) ; if nil, italics is universally disabled
                         ;; doom-one, doom-vibrant, doom-acario-dark
                         ;; doom-one-light, doom-acario-light,
                         ;; doom-city-lights, doom-dracula
                         ;; (load-theme 'doom-city-lights t)

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
                              (setq dumb-jump-selector 'helm
                                    dumb-jump-prefer-searcher 'rg)))

         (:name dwim-shell-command
                :after (progn (require 'dwim-shell-commands)
                              (define-key dired-mode-map
                                          [remap shell-command]
                                          #'dwim-shell-command)
                              (define-key dired-mode-map
                                          [remap dired-do-async-shell-command]
                                          #'dwim-shell-command)
                              (define-key dired-mode-map
                                          [remap dired-do-shell-command]
                                          #'dwim-shell-command)
                              (define-key dired-mode-map
                                          [remap dired-smart-shell-command]
                                          #'dwim-shell-command)

                              (defun dwim-shell-commands-copy-remote-to-downloads ()
                                (interactive)
                                (dwim-shell-command-on-marked-files
                                 "Copy remote to local Downloads"
                                 "scp '<<f>>' ~/Downloads/"
                                 :utils "scp"
                                 :post-process-template
                                 (lambda (script file)
                                   ;; Tramp file path start with "/ssh:". Drop it.
                                   (string-replace file
                                                   (string-remove-prefix "/ssh:" file)
                                                   script))))))

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

         (:name elfeed
                :after (progn
                         (require 'elfeed)

                         (setq elfeed-feeds
                               '(("http://www.50ply.com/atom.xml" gamedesign programming)
                                 ;; Alexander's site is gorgeous!
                                 ("https://alexanderobenauer.com/assets/feed/rss.xml" operating_systems programming)
                                 ("https://andrewkelley.me/rss.xml" foss zig)
                                 ("https://apenwarr.ca/log/rss.php" important leadership)
                                 ("http://arches.io/feed.xml" leadership)
                                 ("https://biffweb.com/feed.xml" biff jacob_obryant)
                                 ("https://blog.lohr.dev/rss.xml" rust zig)
                                 ("https://blog.tecosaur.com/tmio/rss.xml" emacs important orgmode)
                                 ("https://borretti.me/feed.xml" important metalearning)
                                 ("https://buttondown.email/thehighpony/rss" bobdoto taking_notes)
                                 ("https://bzg.fr/en/index.xml" emacs important)
                                 ("https://clojure.org/feed.xml" clojure important)
                                 ("https://corfield.org/atom.xml" clojure programming)
                                 ("https://danb.me/blog/index.xml" opensource)
                                 ("https://dragan.rocks/feed.xml" clojure machine_learning)
                                 ("https://drewdevault.com/blog/index.xml" programming opensource)
                                 ("https://eieio.games/feed.xml" games programming)
                                 ("https://evertpot.com/atom.xml" programming web_development)
                                 ("http://feeds.feedburner.com/BenNorthrop" programming)
                                 ("https://feeds.feedburner.com/martinkl" distributed_systems programming)
                                 ("https://grishaev.me/feed.xml" clojure programming)
                                 ("https://www.gwern.net/docs/personal/rss-subscriptions.opml" longreads)
                                 ("https://ideolalia.com/feed.xml" programming)
                                 ("https://infrequently.org/feed" programming web_development)
                                 ("https://josem.co/articles/index.xml" software)
                                 ("https://www.kellblog.com/feed" leadership)
                                 ("https://leahneukirchen.org/trivium/index.atom" recommendations)
                                 ("https://ludic.mataroa.blog/rss/" rants)
                                 ("https://martinfowler.com/feed.atom" bliki distributed_systems important programming)
                                 ("https://mcfunley.com/feed.xml" programming)
                                 ("https://mtlynch.io/posts/index.xml" programming ziglang)
                                 ("http://nullprogram.com/feed/" emacs)
                                 ("https://oremacs.com/atom.xml" emacs)
                                 ("http://planet.clojure.in/atom.xml" clojure programming)
                                 ("https://randsinrepose.com/feed/" leadership rands)
                                 ("https://sachachua.com/blog/feed" emacs)
                                 ("https://www.scattered-thoughts.net/atom.xml" hytradboi important programming)
                                 ("https://scottjehl.com/feed.xml" programming web_development)
                                 ("https://tech.redplanetlabs.com/feed/" clojure distributed_systems programming)
                                 ("https://theheretic.org/feed.xml" leadership)
                                 ("http://tonsky.me/blog/atom.xml" clojure important rants programming)
                                 ("https://tsdh.org/rss.xml" emacs)
                                 ("https://vanderburg.org/feed.xml" programming)
                                 ("https://vedang.me/feeds/feed.xml" important)
                                 ("https://waitbutwhy.com/feed" longreads)
                                 ("https://xenodium.com/rss.xml" xenodium emacs)
                                 ("https://ziglang.org/devlog/index.xml" ziglang)))

                         ;; Mark all YouTube entries
                         (add-hook 'elfeed-new-entry-hook
                                   (elfeed-make-tagger :feed-url "youtube\\.com"
                                                       :add '(video youtube)))

                         ;; Entries older than 4 weeks are marked as read
                         (add-hook 'elfeed-new-entry-hook
                                   (elfeed-make-tagger :before "4 weeks ago"
                                                       :remove 'unread))

                         (defface important-elfeed-entry
                           '((t :foreground "#f77"))
                           "Marks an important Elfeed entry.")

                         (push '(important important-elfeed-entry)
                               elfeed-search-face-alist)

                         (defface reference-elfeed-entry
                           '((t :foreground "#0373fc"))
                           "Marks an Elfeed entry as something I want in my reference notes.")

                         (push '(reference reference-elfeed-entry)
                               elfeed-search-face-alist)))

         (:name ef-themes
                :after
                (progn (setq ef-themes-to-toggle '(ef-elea-light ef-elea-dark)
                             ef-themes-variable-pitch-ui t
                             ef-themes-mixed-fonts t
                             ef-themes-headings
                             '((0 . (variable-pitch light 1.9))
                               (1 . (variable-pitch light 1.8))
                               (2 . (variable-pitch regular 1.7))
                               (3 . (variable-pitch regular 1.6))
                               (4 . (variable-pitch regular 1.5))
                               (5 . (variable-pitch 1.4)) ; absence of weight means `bold'
                               (6 . (variable-pitch 1.3))
                               (7 . (variable-pitch 1.2))
                               (agenda-date . (semilight 1.5))
                               (agenda-structure . (variable-pitch light 1.9))
                               (t . (variable-pitch 1.1))))
                       (ef-themes-select 'ef-elea-light)))

         (:name elixir-ts-mode)

         (:name elm-mode)

         (:name elpher)

         ;; (:name emacs-dashboard
         ;;        :after (progn (dashboard-setup-startup-hook)
         ;;                      (setq dashboard-projects-backend 'project-el)
         ;;                      (add-to-list 'dashboard-items '(projects . 5))))

         ;; Easy kill might remove the complete need of `change-inner'
         ;; and `expand-region'. I'll observe for a bit and then take
         ;; the call of whether to keep the changes in or not.
         (:name easy-kill
                :after (progn (global-set-key [remap kill-ring-save] 'easy-kill)
                              (global-set-key [remap mark-sexp] 'easy-mark)))

         (:name fancy-battery
                :after (progn (setq fancy-battery-show-percentage t)
                              (add-hook 'after-init-hook #'fancy-battery-mode)))

         (:name fontaine
                :after (progn (add-hook 'kill-emacs-hook #'fontaine-store-latest-preset)
                              (dolist (hook '(modus-themes-after-load-theme-hook ef-themes-post-load-hook))
                                (add-hook hook #'fontaine-apply-current-preset))
                              (setq fontaine-presets
                                    '((tiny
                                       :default-family "Iosevka Comfy Wide Fixed"
                                       :default-height 170)
                                      (small
                                       :default-family "Iosevka Comfy Fixed"
                                       :default-height 190)
                                      (regular
                                       :default-height 200)
                                      (medium
                                       :default-height 210)
                                      (large
                                       :default-weight semilight
                                       :default-height 250
                                       :bold-weight extrabold)
                                      (presentation
                                       :default-weight semilight
                                       :default-height 330
                                       :bold-weight extrabold)
                                      (jumbo
                                       :default-weight semilight
                                       :default-height 380
                                       :bold-weight extrabold)
                                      (t
                                       ;; I keep all properties for didactic purposes, but most can be
                                       ;; omitted.  See the fontaine manual for the technicalities:
                                       ;; <https://protesilaos.com/emacs/fontaine>.
                                       :default-family "Iosevka Comfy"
                                       :default-weight regular
                                       :default-height 200
                                       :fixed-pitch-family nil ; falls back to :default-family
                                       :fixed-pitch-weight nil ; falls back to :default-weight
                                       :fixed-pitch-height 1.0
                                       :fixed-pitch-serif-family nil ; falls back to :default-family
                                       :fixed-pitch-serif-weight nil ; falls back to :default-weight
                                       :fixed-pitch-serif-height 1.0
                                       :variable-pitch-family "Iosevka Comfy Duo"
                                       :variable-pitch-weight nil
                                       :variable-pitch-height 1.0
                                       :bold-family nil ; use whatever the underlying face has
                                       :bold-weight bold
                                       :italic-family nil
                                       :italic-slant italic
                                       :line-spacing nil)))

                              (fontaine-set-preset (or (fontaine-restore-latest-preset) 'medium))))

         (:name forge
                :after (progn (setq auth-sources '("~/.authinfo.gpg"))
                              (with-eval-after-load 'magit
                                (require 'forge))))

         (:name flycheck
                :after (progn (setq flycheck-global-modes '(not org-mode)
                                    flycheck-emacs-lisp-load-path 'inherit)
                              (global-flycheck-mode)))
         (:name flycheck-eglot
                :after (with-eval-after-load 'eglot
                         (global-flycheck-eglot-mode 1)))
         (:name flycheck-inline
                :after (with-eval-after-load 'flycheck
                         (add-hook 'flycheck-mode-hook #'flycheck-inline-mode)))

         (:name gcmh
                :after (progn (require 'gcmh)
                              (gcmh-mode 1)))
         (:name gptel
                :after (progn
                         (setq gptel-api-key #'my/chatgpt-api-key
                               gptel-default-mode 'org-mode)))

         (:name groovy-emacs-mode)

         (:name helm
                :after (progn (require 'init-helm)))

         (:name helm-c-yasnippet
                :after (progn (setq helm-yas-space-match-any-greedy t)
                              (global-set-key (kbd "C-x c y") 'helm-yas-complete)))

         (:name helm-descbinds
                :after (progn (require 'helm-descbinds)
                              (helm-descbinds-mode)))

         ;; (:name helm-notmuch)

         (:name helm-org
                :after (progn (require 'helm-org)
                              (add-to-list 'helm-completing-read-handlers-alist
                                           '(org-capture . helm-org-completing-read-tags))
                              (add-to-list 'helm-completing-read-handlers-alist
                                           '(org-set-tags . helm-org-completing-read-tags))
                              (global-set-key (kbd "C-x c d b")
                                              'helm-org-in-buffer-headings)
                              (global-set-key (kbd "C-x c d a")
                                              'helm-org-agenda-files-headings)))

         (:name helm-system-packages)

         (:name highlight-indentation)

         (:name emacs-humanoid-themes ;; Commenting to use built-in modus
                :after (progn ;; (load-theme 'humanoid-light t)
                         ))

         (:name emacs-fish)

         (:name iedit)

         (:name json-to-org-table)

         (:name linkd)

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

         (:name nov.el
                :after (progn (add-to-list 'auto-mode-alist
                                           '("\\.epub\\'" . nov-mode))))
         (:name ob-mermaid
                :after (progn
                         (setq ob-mermaid-cli-path
                               (expand-file-name "~/node_modules/.bin/mmdc"))))

         (:name org-board
                :after (progn (define-key org-mode-map
                                          (kbd "C-c o") org-board-keymap)))

         ;; (:name org-books
         ;;        ;; My personal settings set up the capture template
         ;;        ;; mentioned in `org-books' README
         ;;        )

         ;; (:name org-brain
         ;;                 :before (progn (autoload 'helm-brain "org-brain")
         ;;                                (global-set-key (kbd "C-c v") 'helm-brain))
         ;;                 :after (progn
         ;;                          ;; Explicit require, because the implicit one
         ;;                          ;; is not working for whatever reason.
         ;;                          (require 'org-brain)
         ;;                          (setq org-brain-visualize-default-choices 'all
         ;;                                org-brain-child-linebreak-sexp '0
         ;;                                org-brain-title-max-length 60
         ;;                                org-brain-include-file-entries nil
         ;;                                org-brain-file-entries-use-title nil
         ;;                                org-brain-show-resources t
         ;;                                org-brain-show-text t
         ;;                                org-brain-visualize-use-capture-templates t
         ;;                                org-brain-scan-directories-recursively t
         ;;                                org-brain-default-file-parent "index"
         ;;                                org-brain-backlink t
         ;;                                org-brain-narrow-to-entry t
         ;;                                org-brain-quit-after-goto t)
         ;; ;;; Key Bindings
         ;;                          (define-key org-mode-map (kbd "C-c b")
         ;;                                      'org-brain-prefix-map)
         ;; ;;; Ensure that all org-mode entries have an ID.
         ;;                          ;;
         ;;                          ;; (add-hook 'before-save-hook
         ;;                          ;;           #'org-brain-ensure-ids-in-buffer)

         ;;                          (remove-hook 'before-save-hook
         ;;                                       #'org-brain-ensure-ids-in-buffer)
         ;; ;;; Configuration to integrate `org-noter'
         ;; ;;; with `org-brain'
         ;;                          (defun org-brain-open-org-noter (entry)
         ;;                            "Open `org-noter' on the ENTRY. If run interactively, get ENTRY from context."
         ;;                            (interactive (list (org-brain-entry-at-pt)))
         ;;                            (org-with-point-at (org-brain-entry-marker entry)
         ;;                              (org-noter)))

         ;;                          (with-eval-after-load 'org-noter
         ;;                            (define-key org-brain-visualize-mode-map
         ;;                                        (kbd "C-c n") 'org-brain-open-org-noter))

         ;;                          ;; Setup `org-expiry' and define a
         ;;                          ;; `org-agenda' function to compare
         ;;                          ;; timestamps. This package is a part of
         ;;                          ;; `org-mode' contrib/lisp folder.
         ;;                          (require 'org-expiry)

         ;;                          (setq org-expiry-inactive-timestamps t)

         ;;                          (defun org-expiry-created-comp (a b)
         ;;                            "Compare `org-expiry-created-property-name' properties of A and B."
         ;;                            (let ((ta (ignore-errors
         ;;                                        (org-time-string-to-seconds
         ;;                                         (org-entry-get (get-text-property 0 'org-marker a)
         ;;                                                        org-expiry-created-property-name))))
         ;;                                  (tb (ignore-errors
         ;;                                        (org-time-string-to-seconds
         ;;                                         (org-entry-get (get-text-property 0 'org-marker b)
         ;;                                                        org-expiry-created-property-name)))))
         ;;                              (cond ((if ta (and tb (< ta tb)) tb) -1)
         ;;                                    ((if tb (and ta (< tb ta)) ta) +1))))

         ;;                          ;; Add CREATED property when adding a new
         ;;                          ;; org-brain headline entry
         ;;                          (add-hook 'org-brain-new-entry-hook
         ;;                                    #'org-expiry-insert-created)

         ;;                          ;; Finally add a function which lets us watch
         ;;                          ;; the entries chronologically
         ;;                          (defun org-brain-timeline ()
         ;;                            "List all org-brain headlines in chronological order."
         ;;                            (interactive)
         ;;                            (let ((org-agenda-files (org-brain-files))
         ;;                                  (org-agenda-cmp-user-defined #'org-expiry-created-comp)
         ;;                                  (org-agenda-sorting-strategy '(user-defined-down)))
         ;;                              (org-tags-view nil (format "+%s>\"\"" org-expiry-created-property-name))))

         ;;                          (defface aa2u-face '((t . nil))
         ;;                            "Face for aa2u box drawing characters"
         ;;                            :group 'org-brain)

         ;;                          (advice-add #'aa2u-1c :filter-return
         ;;                                      (lambda (str) (propertize str 'face 'aa2u-face)))

         ;;                          (defun aa2u-org-brain-buffer ()
         ;;                            (let ((inhibit-read-only t))
         ;;                              (make-local-variable 'face-remapping-alist)
         ;;                              (add-to-list 'face-remapping-alist
         ;;                                           '(aa2u-face . org-brain-wires))
         ;;                              (ignore-errors (aa2u (point-min) (point-max)))))

         ;;                          (with-eval-after-load 'ascii-art-to-unicode
         ;;                            (add-hook 'org-brain-after-visualize-hook #'aa2u-org-brain-buffer))

         ;;                          (defun org-brain-insert-resource-icon (link)
         ;;                            "Insert an icon, based on content of org-mode LINK."
         ;;                            (insert (format "%s "
         ;;                                            (cond ((string-prefix-p "brain:" link)
         ;;                                                   (all-the-icons-fileicon "brain"))
         ;;                                                  ((string-prefix-p "info:" link)
         ;;                                                   (all-the-icons-octicon "info"))
         ;;                                                  ((string-prefix-p "help:" link)
         ;;                                                   (all-the-icons-material "help"))
         ;;                                                  ((string-prefix-p "http" link)
         ;;                                                   (all-the-icons-icon-for-url link))
         ;;                                                  (t
         ;;                                                   (all-the-icons-icon-for-file link))))))

         ;;                          (with-eval-after-load 'all-the-icons
         ;;                            (add-hook 'org-brain-after-resource-button-functions
         ;;                                      #'org-brain-insert-resource-icon))

         ;; ;;; Add a resource to and `org-brain-entry'
         ;; ;;; via `org-cliplink'
         ;;                          (with-eval-after-load 'org-cliplink
         ;;                            (defun org-brain-cliplink-resource ()
         ;;                              "Add a URL from the clipboard as an org-brain resource.
         ;; Suggest the URL title as a description for resource."
         ;;                              (interactive)
         ;;                              (let ((url (org-cliplink-clipboard-content)))
         ;;                                (org-brain-add-resource
         ;;                                 url
         ;;                                 (org-cliplink-retrieve-title-synchronously url)
         ;;                                 t)))

         ;;                            (define-key org-brain-visualize-mode-map (kbd "L")
         ;;                                        'org-brain-cliplink-resource))

         ;;                          (defun org-brain-random-reading ()
         ;;                            "Find something I've already read, to re-read."
         ;;                            (interactive)
         ;;                            (org-brain-visualize-random
         ;;                             (org-brain-headline-entries-in-file
         ;;                              (expand-file-name "linklog.org" org-brain-path))))

         ;;                          (define-key org-brain-visualize-mode-map (kbd "R")
         ;;                                      'org-brain-random-reading)

         ;;                          (setq savehist-additional-variables
         ;;                                '(org-brain-headline-cache))))

         (:name org-cliplink
                :after (require 'org-cliplink))

         (:name org-fc
                :after (progn
                         (require 'org-fc)
                         (setq org-fc-directories
                               `(,(concat org-directory "/notes/")))))

         (:name orgit
                :after (setq orgit-log-save-arguments t))

         (:name orgit-forge)

         (:name org-menu
                :after (progn
                         (with-eval-after-load 'org
                           (define-key org-mode-map (kbd "C-c m") 'org-menu))))
         (:name org-mode-crate
                :after (progn (global-set-key (kbd "C-c a") #'org-agenda)
                              (message "Press `C-c a' to get started with your agenda...")
                              ;; Keep the table of contents on it's own page always
                              (setq org-babel-clojure-backend 'cider)
                              (setq org-html-htmlize-output-type 'css
                                    org-html-head-include-default-style nil)
                              (delete '("\\.pdf\\'" . default) org-file-apps)
                              (add-to-list 'org-file-apps '("\\.pdf\\'" . emacs))
                              (with-eval-after-load 'org
                                (require 'org-mode-crate)
                                (when on-my-machine
                                  (require 'org-crate-config)
                                  (with-eval-after-load 'ox-gfm
                                    (require 'denote-publish))))))

         (:name org-modern
                :after (progn
                         (with-eval-after-load 'org (global-org-modern-mode))))

         (:name org-noter
                :after (progn (add-hook 'org-noter-insert-heading-hook
                                        #'org-id-get-create)))

         (:name org-protocol-capture-html
                :after (progn (require 'org-protocol)
                              (require 'org-protocol-capture-html)))

         (:name org-pomodoro)
         (:name org-pomodoro-third-time
                :after (with-eval-after-load 'org
                         (require 'org-pomodoro-third-time)
                         (setq org-pomodoro-keep-killed-pomodoro-time t
                               org-pomodoro-clock-break t
                               org-pomodoro-length 45)
                         (global-set-key (kbd "C-x c t") #'org-pomodoro)
                         (org-pomodoro-third-time-mode)))

         (:name org-remark
                :before (setq org-remark-create-default-pen-set nil)
                :after (progn
                         ;; Key-bind `org-remark-mark' to global-map
                         ;; so that you can call it globally before
                         ;; the library is loaded.

                         ;; This is originally `revert-buffer'
                         (global-set-key (kbd "C-c r") nil)
                         (global-set-key (kbd "C-c r m") #'org-remark-mark)
                         (with-eval-after-load 'eww
                           (org-remark-eww-mode +1))
                         (with-eval-after-load 'nov
                           (org-remark-nov-mode +1))
                         (with-eval-after-load 'info
                           (org-remark-info-mode +1))
                         ;; The rest of keybidings are done only on
                         ;; loading `org-remark'.
                         (with-eval-after-load 'org-remark
                           (defvar org-remark-locations-file
                             (locate-user-emacs-file "temp-files/.org-remark-locations")
                             "Cache for locations of notes files used by Remark")

                           (defun org-remark-notes-file-name-prompt-function ()
                             "Prompt the user for where we should save marginalia.

Throw an error is the filename is not of type org."
                             (let ((source-filename (org-remark-source-find-file-name)))
                               (read-file-name "Select Remark Notes File: "
                                               nil
                                               (if (and (stringp source-filename)
                                                        (file-exists-p source-filename)
                                                        (string-equal "org" (file-name-extension source-filename)))
                                                   source-filename
                                                 (if (and (stringp source-filename)
                                                          (file-exists-p source-filename)
                                                          (string-equal "org" (file-name-extension source-filename)))
                                                     (concat (file-name-sans-extension
                                                              (file-name-nondirectory source-filename))
                                                             "-notes.org")
                                                   (expand-file-name "marginalia.org" user-emacs-directory))))))

                           (defun vm/org-remark-notes ()
                             (expand-file-name "marginalia.org" org-brain-path))
                           (setq org-remark-notes-file-name
                                 #'vm/org-remark-notes)
                           ;; Create a pen set for specific kinds of
                           ;; highlights. NOTE: This pen-set has been
                           ;; made for dark themes, specifically
                           ;; `humanoid-dark'.
                           (org-remark-create "review"
                                              ;; face: `dired-flagged'
                                              '(:underline
                                                (:color "dark red" :style wave)
                                                :foreground "#f7143a")
                                              '(CATEGORY "review"
                                                         help-echo "Review this"))
                           (define-key org-remark-mode-map (kbd "C-c r e")
                                       #'org-remark-mark-review)

                           (org-remark-create "important"
                                              ;; face: `dired-broken-symlink'
                                              '(:underline "gold"
                                                           :background "red1"
                                                           :foreground "yellow1"
                                                           :weight bold)
                                              '(CATEGORY "important"))
                           (define-key org-remark-mode-map (kbd "C-c r i")
                                       #'org-remark-mark-important)

                           (set-face-bold 'org-remark-highlighter t)
                           (define-key org-remark-mode-map (kbd "C-c r o")
                                       #'org-remark-open)
                           (define-key org-remark-mode-map (kbd "C-c r n")
                                       #'org-remark-view-next)
                           (define-key org-remark-mode-map (kbd "C-c r p")
                                       #'org-remark-view-prev)
                           (define-key org-remark-mode-map (kbd "C-c r r")
                                       #'org-remark-remove)
                           (define-key org-remark-mode-map (kbd "C-c r d")
                                       #'org-remark-delete)
                           (define-key org-remark-mode-map (kbd "C-c r s")
                                       #'org-remark-save)
                           (define-key org-remark-mode-map (kbd "C-c r t")
                                       #'org-remark-toggle)
                           (define-key org-remark-mode-map (kbd "C-c r v")
                                       #'org-remark-view))))

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
                               org-tree-slide-slide-in-blank-lines 10
                               org-tree-slide-skip-comments 'inherit
                               org-tree-slide-skip-outline-level 2)))

         (:name org-web-tools)

         (:name ox-gfm
                :after (with-eval-after-load 'ox
                         (require 'ox-gfm)))

         (:name ox-hugo
                :after (with-eval-after-load 'ox
                         (require 'ox-hugo)))

         (:name package-lint)

         ;; Uses hydra to show export options, triggered by C-c /
         (:name pandoc-mode)

         (:name pcre2el)

         (:name pdf-tools
                :after (progn (pdf-tools-install)
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

         (:name propcheck)

         (:name qpdf)

         (:name rainbow-mode)

         (:name restclient)

         (:name rfc-mode
                :after (progn
                         (setq rfc-mode-directory
                               (concat (expand-file-name "rfc" dotfiles-dirname) "/"))))

         (:name saveplace-pdf-view)

         (:name separedit
                :after (progn
                         (define-key prog-mode-map (kbd "C-c '") #'separedit)
                         (define-key minibuffer-local-map (kbd "C-c '") #'separedit)
                         (define-key help-mode-map (kbd "C-c '") #'separedit)
                         (setq separedit-default-mode 'markdown-mode)))

         (:name shrface
                :after (progn
                         (with-eval-after-load 'eww
                           (require 'shrface)
                           (add-hook 'eww-after-render-hook #'shrface-mode))
                         (with-eval-after-load 'nov
                           (require 'shrface)
                           (setq nov-shr-rendering-functions
                                 (append nov-shr-rendering-functions
                                         shr-external-rendering-functions))
                           (add-hook 'nov-mode-hook #'shrface-mode))
                         (with-eval-after-load 'anki
                           (require 'shrface)
                           (setq anki-shr-rendering-functions
                                 (append anki-shr-rendering-functions
                                         shr-external-rendering-functions))
                           (setq sql-sqlite-program
                                 (executable-find "sqlite3"))
                           (setq anki-collection-dir
                                 "/Users/nejo/Library/Application Support/Anki2/nejo")
                           (add-hook 'anki-mode-hook #'shrface-mode)
                           (autoload 'anki "anki")
                           (autoload 'anki-browser "anki")
                           (autoload 'anki-list-decks "anki"))

                         (with-eval-after-load 'shrface
                           (shrface-basic)
                           (shrface-trial)
                           (shrface-default-keybindings)
                           (setq shrface-href-versatile t)

                           (define-key shrface-mode-map
                                       (kbd "M-l") 'shrface-links-helm)
                           (define-key shrface-mode-map
                                       (kbd "M-h") 'shrface-headline-helm))))

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

         (:name spacious-padding
                :after (progn (setq spacious-padding-subtle-mode-line t)
                              (spacious-padding-mode +1)))

         (:name tagedit
                :after (with-eval-after-load 'sgml-mode
                         (require 'tagedit)
                         (defun turn-on-tagedit ()
                           (tagedit-add-paredit-like-keybindings)
                           (tagedit-add-experimental-features)
                           (tagedit-mode +1))
                         (add-hook 'html-mode-hook #'turn-on-tagedit)))

         (:name theme-buffet
                :after (let ((modus-themes-p (featurep 'modus-themes))
                             (ef-themes-p (featurep 'ef-themes)))
                         (setq theme-buffet-menu 'end-user)
                         (setq theme-buffet-time-offset 0)
                         (setq theme-buffet-end-user
                               '(:night (ef-dark ef-winter ef-autumn ef-night ef-duo-dark ef-symbiosis ef-owl)
                                        :morning   (ef-light ef-cyprus ef-spring ef-frost ef-duo-light ef-eagle)
                                        :afternoon (ef-arbutus ef-day ef-kassio ef-summer ef-elea-light ef-maris-light ef-melissa-light ef-trio-light ef-reverie)
                                        :evening   (ef-rosa ef-elea-dark ef-maris-dark ef-melissa-dark ef-trio-dark ef-dream)))

                         (when (or modus-themes-p ef-themes-p)
                           (theme-buffet-timer-hours 2))))
         (:name ts
                :after (progn (require 'ts)))

         (:name unicode-fonts
                :after (progn (unicode-fonts-setup)))

         (:name vterm
                :after (progn (global-set-key (kbd "C-x M") #'vterm)))

         (:name writegood)

         (:name xterm-color
                :after (progn (require 'xterm-color)
                              (add-hook 'comint-preoutput-filter-functions
                                        #'xterm-color-filter)
                              (setq comint-output-filter-functions
                                    (remove 'ansi-color-process-output
                                            comint-output-filter-functions))))

         (:name yaml-mode)

         (:name yasnippet
                :after (progn (yas-global-mode 1)
                              (add-to-list 'hippie-expand-try-functions-list
                                           'yas-hippie-try-expand)))

         (:name yatemplate
                :after (progn (auto-insert-mode +1)))

         (:name zig-mode)
         (:name zone-sl
                :after (with-eval-after-load 'zone
                         (setq zone-programs
                               (vconcat zone-programs '(zone-pgm-sl)))))
         (:name zone-nyan
                :after (with-eval-after-load 'zone
                         (setq zone-programs
                               (vconcat zone-programs '(zone-nyan))))))))

(defvar el-get-my-packages
  (append

   (when (executable-find "csi")
     '(geiser))

   (when (executable-find "go")
     '(go-mode))

   '(ag
     org-gcal
     org-jira
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
     toml-mode
     unbound
     wgrep
     yasnippet-snippets)

   (mapcar 'el-get-source-name el-get-sources)))

(el-get 'sync el-get-my-packages)

(provide 'init-el-get)
;;; init-el-get.el ends here
