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

           (:name flycheck-clj-kondo)

           (:name jet)))

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

	      '((:name all-the-icons)

            (:name all-the-icons-ibuffer
                   :after (progn (all-the-icons-ibuffer-mode 1)))

            (:name all-the-icons-dired
                   :after (progn
                            (add-hook 'dired-mode-hook
                                      #'all-the-icons-dired-mode)))

            (:name org-superstar
                   :after (progn (add-hook 'org-mode-hook
                                           (lambda () (org-superstar-mode 1)))))

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
                                      #'org-mode-prettify-symbols))))))

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

         (:name apheleia
                :after (progn (apheleia-global-mode +1)))

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
                         (add-hook 'python-ts-mode-hook #'combobulate-mode)
                         (add-hook 'js-ts-mode-hook #'combobulate-mode)
                         (add-hook 'css-ts-mode-hook #'combobulate-mode)
                         (add-hook 'yaml-ts-mode-hook #'combobulate-mode)
                         (add-hook 'typescript-ts-mode-hook #'combobulate-mode)
                         (add-hook 'tsx-ts-mode-hook #'combobulate-mode)))
         (:name company-mode
                :after (progn (add-hook 'after-init-hook #'global-company-mode)
                              (setq company-require-match nil
                                    company-tooltip-align-annotations t)
                              (with-eval-after-load 'company
                                (define-key company-active-map
                                  (kbd "TAB") 'company-complete))))

         (:name company-auctex)

         (:name company-ansible)

         (:name denote
                :after (progn
                         (setq denote-directory
                               (expand-file-name "~/Tresors/Documents/diary/notes")
                               denote-date-prompt-use-org-read-date t
                               denote-backlinks-show-context t)

                         (defvar my-denote-silo-directories
                           `(,(expand-file-name "~/Tresors/Documents/salher-content/docs")
                             ;; You don't actually need to include the
                             ;; `denote-directory' here if you use the
                             ;; regular commands in their global
                             ;; context. I am including it for
                             ;; completeness.
                             ,denote-directory)
                           "List of file paths pointing to my Denote silos.")

                         (defvar my-denote-commands-for-silos
                           '(denote
                             denote-date
                             denote-subdirectory
                             denote-template
                             denote-type
                             denote-signature)
                           "List of commands to call after selecting a silo.")

                         (defun my-denote-pick-silo-then-command (silo command)
                           "Select SILO and run Denote COMMAND in it.
SILO is a file path from `my-denote-silo-directories', while
COMMAND is one among `my-denote-commands-for-silos'."
                           (interactive
                            (list (completing-read "Select a silo: " my-denote-silo-directories nil t)
                                  (intern (completing-read
                                           "Run command in silo: "
                                           my-denote-commands-for-silos nil t))))
                           (let ((denote-directory silo))
                             (call-interactively command)))

                         (defun my-denote-open-or-create (&optional ask-silo)
                           "Select SILO and run Denote COMMAND in it.
SILO is a file path from `my-denote-silo-directories', while
COMMAND is one among `my-denote-commands-for-silos'."
                           (interactive
                            (list (when current-prefix-arg
                                    (completing-read "Select a silo: " my-denote-silo-directories nil t))))
                           (let ((denote-directory (or ask-silo denote-directory)))
                             (call-interactively #'denote-open-or-create)))

                         (setq denote-dired-directories
                               my-denote-silo-directories
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
                         (add-hook 'find-file-hook
                                   #'denote-link-buttonize-buffer)
                         (add-hook 'dired-mode-hook
                                   #'denote-dired-mode-in-directories)

                         (denote-rename-buffer-mode 1)

                         ;; Register Denote's Org dynamic blocks
                         (require 'denote-org-dblock)

                         ;; I use Yasnippet to expand these into a
                         ;; better template.
                         (setq denote-templates
                               '((journal . "checkin")))

                         (defun my-denote-journal ()
                           "Create a new journal entry."
                           (interactive)
                           (denote
                            ;; format like Tuesday 14 June 2022
                            (format-time-string "%A %e %B %Y")
                            nil nil ; no need for keywords or file-type
                            ;; Specify the subdirectory
                            (expand-file-name "journal" denote-directory)
                            nil ; no need for date
                            'journal))

                         (defun my-denote-create-new-note-from-region (beg end)
                           "Create note whose contents include the text between BEG and END.
Prompt for title and keywords of the new note."
                           (interactive "r")
                           (if-let (((region-active-p))
                                    (text (buffer-substring-no-properties beg end)))
                               (progn
                                 (denote (denote-title-prompt) (denote-keywords-prompt))
                                 (insert text))
                             (user-error "No region is available")))


                         (defun my-org-entry-end-position ()
                           "Return the end position of the current subtree."
                           (save-excursion (org-end-of-subtree t) (point)))

                         (defun my-denote-org-extract-subtree ()
                           "Create new Denote note using current Org subtree.
Make the new note use the Org file type, regardless of the value
of `denote-file-type'.

Use the subtree title as the note's title.  If available, use the
tags of the heading are used as note keywords.

Delete the original subtree."
                           (interactive)
                           (if-let ((text (org-get-entry))
                                    (heading (org-get-heading :no-tags :no-todo :no-priority :no-comment)))
                               (let ((element (org-element-at-point))
                                     (tags (org-get-tags)))
                                 (delete-region (org-entry-beginning-position) (my-org-entry-end-position))
                                 (denote heading
                                         tags
                                         'org
                                         nil
                                         (or
                                          (org-element-property :DATE element)
                                          (org-element-property :CREATED element)))
                                 (insert text))
                             (user-error "No subtree to extract; aborting")))

                         ;;; Key Bindings
                         ;; Create a new note, or open an existing
                         ;; one. With a prefix argfument, first pick
                         ;; the silo you want to use.
                         (global-set-key (kbd "C-c d n")
                                         #'my-denote-open-or-create)
                         ;; Create a new note, specifying where it
                         ;; goes and what type it is. Useful when you
                         ;; want to run a specific denote command that
                         ;; is not on any other keybinding.
                         (global-set-key (kbd "C-c d N")
                                         #'my-denote-pick-silo-then-command)
                         ;; Link to an existing note or create a new one
                         (global-set-key (kbd "C-c d l")
                                         #'denote-link-or-create)
                         ;; Display the backlinks buffer
                         (global-set-key (kbd "C-c d B")
                                         #'denote-link-backlinks)
                         ;; Visit a backlink directly
                         (global-set-key (kbd "C-c d b")
                                         #'denote-link-find-backlink)
                         ;; Visit a forwardlink directly
                         (global-set-key (kbd "C-c d f")
                                         #'denote-link-find-file)
                         ;; Write a new journal entry
                         (global-set-key (kbd "C-c d j")
                                         #'my-denote-journal)

                         ;; Key bindings specifically for Dired.
                         (let ((map dired-mode-map))
                           (define-key map (kbd "C-c C-d C-i")
                                       #'denote-link-dired-marked-notes)
                           (define-key map (kbd "C-c C-d C-r")
                                       #'denote-dired-rename-marked-files)
                           (define-key map (kbd "C-c C-d C-R")
                                       #'denote-dired-rename-marked-files-using-front-matter))))
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
                              (apheleia . apheleia-mode))
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
                         ;; (load-theme 'doom-one t)

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

         (:name eglot
                :after (with-eval-after-load 'eglot
                         (defclass eglot-deno (eglot-lsp-server) ()
                           :documentation "A custom class for deno lsp.")

                         (cl-defmethod eglot-initialization-options ((server eglot-deno))
                           "Passes through required deno initialization options"
                           (list :enable t
                                 :lint t))

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
			 (add-hook 'go-mode-hook #'eglot-ensure)
			 (defun eglot-format-buffer-on-save ()
			   (add-hook 'before-save-hook #'eglot-format-buffer -10 t))
			 (add-hook 'go-mode-hook #'eglot-format-buffer-on-save)))

         (:name elfeed
                :after (progn
                         (require 'elfeed)

                         (setq elfeed-feeds
                               '(("http://nullprogram.com/feed/" emacs)
                                 ("https://tsdh.org/rss.xml" emacs tassilo_horn)
                                 ("https://sachachua.com/blog/feed" emacs)
                                 "http://www.50ply.com/atom.xml"
                                 ("https://www.gwern.net/docs/personal/rss-subscriptions.opml" longreads)
                                 ("https://waitbutwhy.com/feed" longreads)
                                 ("https://oremacs.com/atom.xml" emacs)
                                 ("https://tech.redplanetlabs.com/feed/" clojure distributed_systems)
                                 ("http://planet.clojure.in/atom.xml" planet_clojure)
                                 ("https://corfield.org/atom.xml" clojure sean_corfield)
                                 ("https://dragan.rocks/feed.xml" clojure dragan machine_learning)
                                 ("https://ideolalia.com/feed.xml" zach_tellman)
                                 ("https://mcfunley.com/feed.xml" dan_mckinley)
                                 ("https://feeds.feedburner.com/martinkl" distributed_systems)
                                 ("https://blog.tecosaur.com/tmio/rss.xml" org_mode emacs important)
                                 ("https://bzg.fr/en/index.xml" bzg emacs important)
                                 ("https://martinfowler.com/feed.atom" bliki important)
                                 ("https://drewdevault.com/blog/index.xml" programming)
                                 ("https://www.kellblog.com/feed" dave_kellogg management)
                                 ("https://evertpot.com/atom.xml" web_development)
                                 ("https://vanderburg.org/feed.xml" software_engineering)
                                 ("https://clojure.org/feed.xml" clojure important)
                                 ("http://tonsky.me/blog/atom.xml" clojure software_engineering)
                                 ("https://www.scattered-thoughts.net/atom.xml" hytradboi jamie_brandon)
                                 ("https://vedang.me/feed.xml" personal important)
                                 ("https://theheretic.org/feed.xml" leadership management)
                                 ("https://randsinrepose.com/feed/" rands leadership management)
                                 ("https://xenodium.com/rss.xml" xenodium emacs)
                                 ;; Alexander's site is gorgeous!
                                 ("https://alexanderobenauer.com/assets/feed/rss.xml" operating_systems programming)
                                 ("https://buttondown.email/thehighpony/rss" bobdoto)))

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
                               elfeed-search-face-alist)))


         (:name elpher)


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
         (:name gptel
                :after (progn
                         (setq gptel-api-key #'my/chatgpt-api-key)))

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

         (:name helm-system-packages)

         (:name highlight-indentation)

         (:name emacs-humanoid-themes
                :after (progn (load-theme 'humanoid-dark t)))

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

         (:name org-cliplink
                :after (require 'org-cliplink))

         (:name org-fc
                :after (progn
                         (require 'org-fc)
                         (setq org-fc-directories
                               `(,(concat org-directory "/flashcards/")))))

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
                              (setq org-latex-toc-command "\\tableofcontents \\clearpage"
                                    org-babel-clojure-backend 'cider
                                    org-latex-with-hyperref
                                    "\\hypersetup{
 pdfauthor={%a},
 pdftitle={%t},
 pdfkeywords={%k},
 pdfsubject={%d},
 pdfcreator={%c},
 pdflang={%L},
 linktoc=all,
 colorlinks=true
 }
")
                              (add-to-list 'org-latex-default-packages-alist
                                           "\\PassOptionsToPackage{hyphens}{url}")
                              (setq org-html-htmlize-output-type 'css
                                    org-html-head-include-default-style nil)
                              (delete '("\\.pdf\\'" . default) org-file-apps)
                              (add-to-list 'org-file-apps '("\\.pdf\\'" . emacs))
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

         (:name org-pomodoro)
         (:name org-pomodoro-third-time
                :after (progn (require 'org-pomodoro-third-time)
                              (setq org-pomodoro-keep-killed-pomodoro-time t
                                       org-pomodoro-clock-break t)
                              (global-set-key (kbd "C-x c o p")
                                              #'org-pomodoro)
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
                         (org-remark-global-tracking-mode +1)
                         (with-eval-after-load 'eww
                           (org-remark-eww-mode +1))
                         ;; The rest of keybidings are done only on
                         ;; loading `org-remark'.
                         (with-eval-after-load 'org-remark
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
                           (define-key org-remark-mode-map (kbd "C-c r y")
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
                           (define-key org-remark-mode-map (kbd "C-c r s")
                                       #'org-remark-save)
                           (define-key org-remark-mode-map (kbd "C-c r l")
                                       #'org-remark-highlights-load)
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
                               org-tree-slide-slide-in-blank-lines 10)))

         (:name org-web-tools)

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

         (:name saveplace-pdf-view)

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
                             (kbd "M-h") 'shrface-headline-helm)
                           (set-face-attribute 'variable-pitch nil
                                               :font "FantasqueSansMono NF 24"))))

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

         (:name smex
                :after (progn
                         (smex-initialize)
                         (global-set-key (kbd "M-X") 'smex-major-mode-commands)
                         (global-set-key (kbd "M-x") 'smex)))

         (:name solaire-mode
                :after (solaire-global-mode +1))

         (:name tagedit
                :after (with-eval-after-load 'sgml-mode
                         (require 'tagedit)
                         (defun turn-on-tagedit ()
                           (tagedit-add-paredit-like-keybindings)
                           (tagedit-add-experimental-features)
                           (tagedit-mode +1))
                         (add-hook 'html-mode-hook #'turn-on-tagedit)))

         (:name ts
                :after (progn (require 'ts)))

         (:name unicode-fonts
                :after (progn (unicode-fonts-setup)))

         (:name vterm)

         (:name writegood)

         (:name xterm-color
                :after (progn (require 'xterm-color)
                              (add-hook 'comint-preoutput-filter-functions
                                        #'xterm-color-filter)
                              (setq comint-output-filter-functions
                                    (remove 'ansi-color-process-output
                                            comint-output-filter-functions))))

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
     toml-mode
     unbound
     wgrep
     yasnippet-snippets)

   (mapcar 'el-get-source-name el-get-sources)))

(el-get 'sync el-get-my-packages)

(provide 'init-el-get)
;;; init-el-get.el ends here
