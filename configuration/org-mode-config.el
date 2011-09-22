;; Configuration for org-mode

(require 'org-install)


(setq org-directory "~/Documents/Notes-GTD"
      org-archive-directory (concat org-directory "/archive")
      org-archive-location (concat org-archive-directory "/%s_archive::")
      org-completion-use-ido t
      org-default-notes-file (concat org-directory "/remember-notes.org")
      org-agenda-files (list
                        org-directory
                        org-archive-directory))


;; Standard org-mode key bindings
(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c t") 'org-todo)
(global-set-key (kbd "C-c C-r") 'org-capture)
(global-set-key (kbd "<f6>") 'org-capture)
(global-set-key (kbd "<S-f5>") 'widen)
(global-set-key (kbd "<f11>") 'org-clock-goto)
(global-set-key (kbd "<f12>") 'org-agenda)
(global-set-key (kbd "C-<f11>") 'org-clock-in)
(global-set-key (kbd "<f9> SPC") 'bh/clock-in-last-task)
(global-set-key (kbd "<f9> i") 'bh/clock-in)
(global-set-key (kbd "<f9> o") 'bh/clock-out)


;; Auto starting org-mode for following file types
(add-to-list 'auto-mode-alist '("\\.\\(org\\|org_archive\\|txt\\)$" . org-mode))
(transient-mark-mode 1)

;; Undefine C-c [ and C-c ] since this breaks my org-agenda files
;; when directories are included
;; It expands the files in the directories individually
(add-hook 'org-mode-hook (lambda ()
                           (turn-on-font-lock)
                           (org-defkey org-mode-map "\C-c[" 'undefined)
                           (org-defkey org-mode-map "\C-c]" 'undefined)))


;; Settings for org-capture
(setq org-capture-templates
      (quote
       (("t" "todo" entry
         (file (concat org-directory "/remember-notes.org"))
         "* TODO %?\n%U\n%a\n %i" :clock-in t :clock-resume t)
        ("n" "note" entry
         (file (concat org-directory "/remember-notes.org"))
         "* %?  :NOTE:\n%U\n%a\n  %i" :clock-in t :clock-resume t)
        ("l" "linklog" entry
         (file (concat org-directory "/linklog.org"))
         "* %?\n%U\n%a\n %i" :clock-in t :clock-resume t)
        ("p" "phone" entry
         (file (concat org-directory "/remember-notes.org"))
         "* %?  :PHONE:\n%U\n%a\n %i" :clock-in t :clock-resume t))))


;; refile settings
(setq org-refile-targets (quote ((org-agenda-files :maxlevel . 2)
                                 (nil :maxlevel . 2)))
      org-refile-use-outline-path 'file
      ;; Targets start with the file name - allows creating level 1 tasks
      org-outline-path-complete-in-steps nil
      org-refile-allow-creating-parent-nodes 'confirm)


;; org-todo settings

;; I need more todo keywords than present by default
;; keys mentioned in brackets are hot-keys for the States
;; ! indicates insert timestamp
;; @ indicates insert note
;; / indicates entering the state
(setq org-todo-keywords
      (quote ((sequence "TODO(t!/!)" "WORKING(w!/!)" "REDO(r@/!)" "|"
                        "DONE(d!/@)" "DELEGATED(e@/!)" "MOSTLYDONE(M@/!)")
              (sequence "PROJECT(p)" "LATER(l@/!)" "WAITINGTOMERGE(m/!)"
                        "WAITINGTODEPLOY(a/!)" "|"
                        "SOMEDAY(s)" "CANCELLED(c@/!)")
              (sequence "WEEKEND(W)" "|" "DONE(d!@)"))))


(setq org-todo-keyword-faces
      (quote (("TODO" :foreground "red" :weight bold)
              ("WORKING" :foreground "orange" :weight bold)
              ("REDO" :foreground "magenta" :weight bold)
              ("DONE" :foreground "lightgreen" :weight bold)
              ("DELEGATED" :foreground "lightgreen" :weight bold)
              ("MOSTLYDONE" :foreground "lightblue" :weight bold)
              ("PROJECT" :foreground "lightblue" :weight bold)
              ("TASK" :foreground "darkblue" :weight bold)
              ("WAITINGTOMERGE" :foreground "gray" :weight bold)
              ("WAITINGTODEPLOY" :foreground "gray" :weight bold)
              ("SOMEDAY" :foreground "magenta" :weight bold)
              ("CANCELLED" :foreground "lightgreen" :weight bold))))


;; Changing State should trigger following Tag changes
(setq org-todo-state-tags-triggers
      (quote (("SOMEDAY"
               ("WAITING" . t) ("NEXT"))
              (done
               ("NEXT") ("WAITING"))
              ("WAITINGTOMERGE"
               ("NEXT") ("WAITING" . t))
              ("WAITINGTODEPLOY"
               ("NEXT") ("WAITING" . t))
              ("TODO"
               ("WAITING"))
              ("WORKING"
               ("WAITING") ("NEXT" . t)))))


(setq org-use-fast-todo-selection t
      org-fast-tag-selection-single-key 'expert
      ;; Allow me to change state without it being logged
      org-treat-S-cursor-todo-selection-as-state-change nil
      ;; show TODO counts of _all_ subtasks under a heading
      org-hierarchical-todo-statistics nil
      org-hierarchical-checkbox-statistics nil
      org-enforce-todo-dependencies t)


;; Important Tag list
(setq org-tag-alist (quote ((:startgroup)
                            ("@office" . ?w)
                            ("@home" . ?h)
                            ("@errand" . ?e)
                            (:endgroup)
                            ("LowPri" . ?l)
                            ("NEXT" . ?N)
                            ("RELEASE" . ?r)
                            ("NOTE" . ?n)
                            ("PUBLISHED" . ?p)
                            ("WAITING" . ?a)
                            ("future" . ?f))))


;; I need more priorities that provided by default
(setq org-lowest-priority ?E)
(setq org-default-priority ?E)


;; Logbook settings
(setq org-log-done (quote time)
      org-log-into-drawer t
      org-log-reschedule 'note
      org-log-redeadline 'note)


;; settings for org-clock
(org-clock-persistence-insinuate)
(setq org-clock-history-length 10
      org-clock-in-resume t
      org-drawers (quote ("PROPERTIES" "LOGBOOK" "CLOCK"))
      org-clock-into-drawer "CLOCK"
      org-clock-out-remove-zero-time-clocks t
      org-clock-out-when-done t
      org-clock-persist 'history
      org-clock-persist-file (concat tempfiles-dir "org-clock-save")
      org-clock-auto-clock-resolution 'when-no-clock-is-running
      org-clock-report-include-clocking-task t)


;; Change task state to WORKING when clocking in
(defun bh/clock-in-to-working (kw)
  "Switch task from TODO to WORKING when clocking in.
Skips capture tasks and tasks with subtasks"
  (if (and (string-equal kw "TODO")
           (not (and (boundp 'org-capture-mode) org-capture-mode)))
      (let ((subtree-end (save-excursion (org-end-of-subtree t)))
            (has-subtask nil))
        (save-excursion
          (forward-line 1)
          (while (and (not has-subtask)
                      (< (point) subtree-end)
                      (re-search-forward "^\*+ " subtree-end t))
            (when (member (org-get-todo-state) org-not-done-keywords)
              (setq has-subtask t))))
        (when (not has-subtask)
          "WORKING"))))

(setq org-clock-in-switch-to-state 'bh/clock-in-to-working)


;; Remove empty drawers on clock out
(defun bh/remove-empty-drawer-on-clock-out ()
  (interactive)
  (save-excursion
    (beginning-of-line 0)
    (org-remove-empty-drawer-at "CLOCK" (point))
    (org-remove-empty-drawer-at "LOGBOOK" (point))))

(add-hook 'org-clock-out-hook 'bh/remove-empty-drawer-on-clock-out 'append)


(setq bh/keep-clock-running nil)


(defun bh/clock-in-last-task ()
  "Clock in the interrupted task if there is one"
  (interactive)
  (let ((clock-in-to-task (if (org-clock-is-active)
                              (setq clock-in-to-task (cadr org-clock-history))
                            (setq clock-in-to-task (car org-clock-history)))))
    (org-with-point-at clock-in-to-task
      (org-clock-in nil))))


(defun bh/clock-in ()
  (interactive)
  (setq bh/keep-clock-running t)
  (if (marker-buffer org-clock-default-task)
      (bh/clock-in-default-task)
    (unless (marker-buffer org-clock-default-task)
      (org-agenda nil "c"))))


(defun bh/clock-out ()
  (interactive)
  (setq bh/keep-clock-running nil)
  (when (org-clock-is-active)
    (org-clock-out)))


(defun bh/clock-in-default-task ()
  (save-excursion
    (org-with-point-at org-clock-default-task
      (org-clock-in))))


(defun bh/clock-out-maybe ()
  (when (and bh/keep-clock-running
             (not org-clock-clocking-in)
             (marker-buffer org-clock-default-task))
    (bh/clock-in-default-task)))

(add-hook 'org-clock-out-hook 'bh/clock-out-maybe 'append)


;; Custom views for Agenda
(setq org-agenda-custom-commands
      (quote (("a" "Agenda"
               ((agenda "" nil)
                (tags-todo "RELEASE+@office-WAITING-CANCELLED"
                           ((org-agenda-overriding-header
                             "Release Tasks")
                            (org-agenda-todo-ignore-scheduled t)
                            (org-agenda-todo-ignore-deadlines t)
                            (org-tags-match-list-sublevels t)
                            (org-agenda-sorting-strategy
                             '(effort-up category-keep))))
                (tags-todo "NEXT-RELEASE-WAITING-CANCELLED"
                           ((org-agenda-overriding-header
                             "Next Tasks")
                            (org-agenda-todo-ignore-scheduled t)
                            (org-agenda-todo-ignore-deadlines t)
                            (org-tags-match-list-sublevels t)
                            (org-agenda-sorting-strategy
                             '(effort-up category-keep))))
                (tags "LEVEL=1+REFILE"
                      ((org-agenda-overriding-header
                        "Notes and Tasks to Refile")))
               nil))
              ("c" "Select default clocking task" tags "LEVEL=1-REFILE"
               ((org-agenda-skip-function
                 '(org-agenda-skip-subtree-if 'notregexp "^\\* Organization"))
                (org-agenda-overriding-header
                 "Set default clocking task with C-u C-u I")))
              ("d" "Delegated Tasks" todo "DELEGATED"
                    ((org-use-tag-inheritance nil)
                     (org-agenda-todo-ignore-with-date nil))))))


;; Always highlight current agenda line
(add-hook 'org-agenda-mode-hook '(lambda ()
                                   (hl-line-mode 1)))

(setq org-agenda-repeating-timestamp-show-all nil
      org-agenda-show-all-dates t
      org-agenda-sorting-strategy
      (quote ((agenda time-up priority-down effort-up category-up)
              (todo todo-state-up priority-up)
              (tags priority-down)))
      org-agenda-start-on-weekday nil
      org-agenda-time-grid
      (quote (nil "----------------"
                  (800 1000 1200 1400 1600 1800 2000)))
      org-deadline-warning-days 30
      org-agenda-todo-ignore-with-date t
      org-agenda-skip-deadline-if-done t
      org-agenda-skip-scheduled-if-done t
      org-agenda-text-search-extra-files (quote (agenda-archives))
      org-agenda-log-mode-items (quote (clock))
      org-agenda-clockreport-parameter-plist (quote
                                              (:link nil :maxlevel 3))
      org-agenda-span 1
      org-columns-default-format
      "%80ITEM(Task) %10Effort(Effort){:} %10CLOCKSUM"
      org-global-properties
      (quote (("Effort_ALL" . "0:10 0:30 1:00 2:00 3:00 4:00 5:00 6:00 8:00"))))


;; setup for Reminder
;; Erase all reminders and rebuild reminders for today from the agenda
(defadvice org-agenda-to-appt (before wickedcool activate)
  "Clear the appt-time-msg-list."
  (setq appt-time-msg-list nil))

(add-hook 'org-finalize-agenda-hook 'org-agenda-to-appt)

(appt-activate t)

;; If we leave Emacs running overnight -
;; reset the appointments one minute after midnight
(run-at-time "24:01" nil 'org-agenda-to-appt)

;; Make TAB the yas trigger key in the org-mode-hook and turn on flyspell mode
(add-hook 'org-mode-hook
          (lambda ()
            ;; yasnippet
            (make-variable-buffer-local 'yas/trigger-key)
            (setq yas/trigger-key [tab])
            (define-key yas/keymap [tab] 'yas/next-field-group)
            ;; flyspell mode to spell check everywhere
            (flyspell-mode 1)))


;; Export org table as CSV by default
(setq org-table-export-default-format "orgtbl-to-csv")


;; settings for Beamer
(setq org-ditaa-jar-path (concat dotfiles-dir
                                 "plugins/org-mode/contrib/scripts/ditaa.jar"))
(org-babel-do-load-languages
 'org-babel-load-languages
 '((ditaa . t)
   (dot . t)
   (C . t)
   (emacs-lisp . t)
   (python . t)
   (ruby . t)
   (sh . t)
   (clojure . t)))


(unless (boundp 'org-export-latex-classes)
  (setq org-export-latex-classes nil))
;; allow for export=>beamer by placing #+LaTeX_CLASS: beamer in org files
(add-to-list 'org-export-latex-classes
             ;; beamer class, for presentations
             '("beamer"
               "\\documentclass[11pt]{beamer}\n
      \\mode<{{{beamermode}}}>\n
      \\usetheme{{{{beamertheme}}}}\n
      \\usecolortheme{{{{beamercolortheme}}}}\n
      \\beamertemplateballitem\n
      \\setbeameroption{show notes}
      \\usepackage{color}
      \\usepackage{listings}
      \\lstset{numbers=none,language=[ISO]C++,tabsize=4,
  frame=single,
  basicstyle=\\small,
  showspaces=false,showstringspaces=false,
  showtabs=false,
  keywordstyle=\\color{blue}\\bfseries,
  commentstyle=\\color{red},
  }\n
      \\usepackage{verbatim}\n
      \\institute{{{{beamerinstitute}}}}\n
       \\subject{{{{beamersubject}}}}\n"

               ("\\section{%s}" . "\\section*{%s}")

               ("\\begin{frame}[fragile]\\frametitle{%s}"
                "\\end{frame}"
                "\\begin{frame}[fragile]\\frametitle{%s}"
                "\\end{frame}")))


;; letter class, for formal letters
(add-to-list 'org-export-latex-classes

             '("letter"
               "\\documentclass[11pt]{letter}\n
      \\usepackage{color}\n
      \\usepackage{listings}
      \\lstset{numbers=none,language=[ISO]C++,tabsize=4,
  frame=single,
  basicstyle=\\small,
  showspaces=false,showstringspaces=false,
  showtabs=false,
  keywordstyle=\\color{blue}\\bfseries,
  commentstyle=\\color{red},
  }\n
      \\usepackage{verbatim}\n"

               ("\\section{%s}" . "\\section*{%s}")
               ("\\subsection{%s}" . "\\subsection*{%s}")
               ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
               ("\\paragraph{%s}" . "\\paragraph*{%s}")
               ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))


;; article class, for articles
(add-to-list 'org-export-latex-classes
             '("article"
               "\\documentclass[10pt]{article}
\\usepackage{color}
\\usepackage{listings}
\\lstset{numbers=none,language=[ISO]C++,tabsize=4,
  frame=single,
  basicstyle=\\small,
  showspaces=false,showstringspaces=false,
  showtabs=false,
  keywordstyle=\\color{blue}\\bfseries,
  commentstyle=\\color{red},
  }\n
\\usepackage{verbatim}\n
\\usepackage[left=1in,top=1in,right=1in,bottom=1in,head=0.2in,foot=0.2in]{geometry}"
               ("\\section{%s}" . "\\section*{%s}")
               ("\\subsection{%s}" . "\\subsection*{%s}")
               ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
               ("\\paragraph{%s}" . "\\paragraph*{%s}")
               ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))


;; I don't want org mode to export a_b as a subscript b in latex.
;; I mostly write code documents and this is never the intended behavior
(setq org-export-with-sub-superscripts nil)


;; I want to add comments to my org files without
;; having them show up in the latex export.
(setq org-export-blocks nil)


(add-to-list 'org-export-blocks
             '(src org-babel-exp-src-blocks nil))
(add-to-list 'org-export-blocks
             '(comment org-export-blocks-format-comment nil))
(add-to-list 'org-export-blocks
             '(ditaa org-export-blocks-format-ditaa nil))
(add-to-list 'org-export-blocks
             '(dot org-export-blocks-format-dot nil))
;; ============================================================================


(provide 'org-mode-config)
;; A big thanks to Bernt Hansen for providing an awesome guide to
;; beginners so that we can harness the power of org-mode. Almost all of the
;; customization here, and my complete day-to-day workflow,
;; is based on his document about org-mode which can be
;; found here: http://doc.norang.ca/org-mode.html
