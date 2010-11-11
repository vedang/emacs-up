;Configuration for org-mode

(setq org-directory "~/Documents/Notes-GTD")
(setq diary-directory "~/Documents/Diary")
(setq org-archive-location (concat org-directory "/archive/%s_archive::"))
(setq org-completion-use-ido t)

;; Auto starting org-mode for following file types
(add-to-list 'auto-mode-alist '("\\.\\(org\\|org_archive\\|txt\\)$" . org-mode))
(transient-mark-mode 1)
(add-hook 'org-mode-hook 'turn-on-font-lock)

;; Settings for org-remember
(require 'remember)
(org-remember-insinuate)
(setq org-default-notes-file (concat org-directory "/remember-notes.org"))
(setq org-remember-templates
      `(("Todo"    ?t "* TODO %?\n  %i\n" ,(concat org-directory "/remember-notes.org") bottom)
	("Misc"    ?m "* %?\n  %i\n"      ,(concat org-directory "/Notes.org")   "Misc")
	("iNfo"    ?n "* %?\n  %i\n"      ,(concat org-directory "/Notes.org")   "Information")
	("Idea"    ?i "* %?\n  %i\n"      ,(concat org-directory "/Notes.org")   "Ideas")
	("Journal" ?j "* %T %?\n\n  %i\n" ,(concat diary-directory "/journal.gpg") bottom)
	("Blog"    ?b "* %T %? :BLOG:\n\n  %i\n" ,(concat diary-directory "/other.gpg") bottom)
	))

;; For ease of Re-filing:
;; Targets include this file and any file contributing to the agenda - up to 5 levels deep
(setq org-refile-targets (quote ((org-agenda-files :maxlevel . 5) (nil :maxlevel . 5))))
;; Targets start with the file name - allows creating level 1 tasks
(setq org-refile-use-outline-path (quote file))
;; Targets complete in steps so we start with filename, TAB shows the next level of targets etc
(setq org-outline-path-complete-in-steps t)

;; org-todo settings
;; I need more todo keywords than present by default
;; keys mentioned in brackets are hot-keys for the States
;; ! indicates insert timestamp
;; @ indicates insert note
;; / indicates entering the state
(setq org-todo-keywords
      (quote ((sequence "TODO(t!/!)" "WORKING(w!/!)" "REDO(r@/!)" "|" "DONE(d!/@)" "DEFERRED(e@/!)")
              (sequence "PROJECT(p)" "FINDOUT(f)" "WAITING(a@/!)" "SOMEDAY(s)" "|" "CANCELLED(c@/!)"))))
(setq org-todo-keyword-faces (quote (("TODO" :foreground "red" :weight bold)
 ("WORKING" :foreground "orange" :weight bold)
 ("REDO" :foreground "magenta" :weight bold)
 ("DONE" :foreground "lightgreen" :weight bold)
 ("PROJECT" :foreground "lightblue" :weight bold)
 ("FINDOUT" :foreground "red" :weight bold)
 ("WAITING" :foreground "gray" :weight bold)
 ("SOMEDAY" :foreground "magenta" :weight bold)
 ("CANCELLED" :foreground "lightgreen" :weight bold))))

;; Changing State should trigger following Tag changes
(setq org-todo-state-tags-triggers
      (quote (("SOMEDAY" ("WAITING" . t) ("NEXT"))
              (done ("NEXT") ("WAITING"))
	      ("WAITING" ("NEXT") ("WAITING" . t))
              ("TODO" ("WAITING"))
              ("WORKING" ("WAITING") ("NEXT" . t)))))
;; Faster state set
(setq org-use-fast-todo-selection t)

;; Important Tag list
(setq org-tag-alist (quote ((:startgroup)
                            ("@Work" . ?w)
			    ("@Dreamz" . ?d)
			    ("@Gate" . ?g)
			    (:endgroup)
			    ("NOTIMP" . ?i)
			    ("LEGIT" . ?l)
			    ("NEXT" . ?N)
			    ("NOTE" . ?n)
                            ("PUBLISHED" . ?p)
                            ("MRD" . ?m)
			    ("CURR_ITERATION" . ?c)
			    ("WAITING" . ?a))))

;; I need more priorities that provided by default
(setq org-lowest-priority ?E)
(setq org-default-priority ?E)

;; Logbook settings
(setq org-log-done (quote time))
(setq org-log-into-drawer t)

;; settings for org-clock
;; Resume clocking tasks when emacs is restarted
(org-clock-persistence-insinuate)
(setq org-clock-history-length 35)
;; Resume clocking task on clock-in if the clock is open
(setq org-clock-in-resume t)
;; Change task state to WORKING when clocking in
(setq org-clock-in-switch-to-state "WORKING")
;; Save clock data and notes in the LOGBOOK drawer
(setq org-clock-into-drawer t)
;; Sometimes I change tasks I'm clocking quickly - this removes clocked tasks with 0:00 duration
(setq org-clock-out-remove-zero-time-clocks t)
;; Save the running clock and all clock history when exiting Emacs, load it on startup
(setq org-clock-persist t)
(setq org-clock-persist-file "~/.emacs.d/org-clock-save")
;; Agenda log mode items to display (clock time only by default)
(setq org-agenda-log-mode-items (quote (clock)))
;; Agenda clock report parameters (no links, 2 levels deep)
(setq org-agenda-clockreport-parameter-plist (quote (:link nil :maxlevel 2)))

;; settings for estimates and efforts
;; Set default column view headings: Task Effort Clock_Summary
(setq org-columns-default-format "%80ITEM(Task) %10Effort(Effort){:} %10CLOCKSUM")
;; global Effort estimate values
(setq org-global-properties (quote (("Effort_ALL" . "0:10 0:30 1:00 2:00 3:00 4:00 5:00 6:00 8:00"))))

;; Custom views for Agenda
(setq org-agenda-custom-commands
      (quote (("d" "Started Tasks" todo "WORKING" ((org-agenda-todo-ignore-with-date nil)))
              ("q" "Tasks waiting on something" tags "WAITING" ((org-use-tag-inheritance nil)))
	      ("w" "Work Tasks" tags-todo "+@Work" ((org-use-tag-inheritance t)))
              ("r" "Refile New Notes and Tasks" tags "REFILE" ((org-agenda-todo-ignore-with-date nil)))
              ("i" "Important Tasks" tags-todo "-NOTIMP" ((org-agenda-todo-ignore-with-date t))))))

;; Always highlight current agenda line
(add-hook 'org-agenda-mode-hook '(lambda ()
				   (hl-line-mode 1)))
;; Don't show future entries for repeating tasks
(setq org-agenda-repeating-timestamp-show-all nil)
;; Show all agenda dates - even if empty
(setq org-agenda-show-all-dates t)
;; Sorting order for tasks on the agenda
(setq org-agenda-sorting-strategy
      (quote ((agenda time-up priority-down effort-up category-up)
              (todo todo-state-up priority-up)
              (tags priority-down))))
;; Start the weekly agenda today
(setq org-agenda-start-on-weekday nil)
;; Disable display of the time grid
(setq org-agenda-time-grid
      (quote (nil "----------------"
                  (800 1000 1200 1400 1600 1800 2000))))
;; Gimme 30 days warning of deadlines
(setq org-deadline-warning-days 30)
;; Keep tasks with dates off the global todo lists
(setq org-agenda-todo-ignore-with-date t)
;; Remove completed deadline tasks from the agenda view
(setq org-agenda-skip-deadline-if-done t)
;; Remove completed scheduled tasks from the agenda view
(setq org-agenda-skip-scheduled-if-done t)
;; Include agenda archive files when searching for things
(setq org-agenda-text-search-extra-files (quote (agenda-archives)))
;; Block task until all subtasks are in a done todo state
(setq org-enforce-todo-dependencies t)

;from Sacha Chua's emacs config.
;; This code has been invalidated by org-write-agenda.
;; Currently, I have no need to save the agenda to a file,
;; this is not part of my workflow.
;; Keeping this because I will need it some time in the future
;; (defun vedang/org-publish-agenda ()
;;   "Copy the agenda buffer to a text file."
;;   (interactive)
;;   ;; Take the entire contents of the agenda and dump it into a text file labeled with the date.
;;   (let ((agenda (with-current-buffer org-agenda-buffer-name
;;                   (unless org-agenda-show-log (org-agenda-log-mode))
;;                   (buffer-string)))
;; 	(filename (format-time-string "StatusReport-%Y-%m-%d.txt" (if org-starting-day
;; 								      (calendar-time-from-absolute (1+ org-starting-day) 0)
;; 								    (current-time)))))
;;     (with-temp-buffer
;;       (insert agenda)
;;       (write-file (expand-file-name filename
;;                                     "~/Work")))))

;; Standard org-mode key bindings
(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c t") 'org-todo)
(global-set-key (kbd "C-c r") 'org-remember)
(global-set-key (kbd "<f5>") 'my-org-todo)
(global-set-key (kbd "<S-f5>") 'widen)
(global-set-key (kbd "<f11>") 'org-clock-goto)
(global-set-key (kbd "<f12>") 'org-agenda)
(global-set-key (kbd "C-<f11>") 'org-clock-in)

;; function to narrow view-field and make org-file more productive
(defun my-org-todo ()
  (interactive)
  (org-narrow-to-subtree)
  (org-show-todo-tree nil)
  (widen))

;; setup for Reminder
;; Erase all reminders and rebuild reminders for today from the agenda
(defadvice org-agenda-to-appt (before wickedcool activate)
  "Clear the appt-time-msg-list."
  (setq appt-time-msg-list nil))
(add-hook 'org-finalize-agenda-hook 'org-agenda-to-appt)
(appt-activate t)

;; If we leave Emacs running overnight - reset the appointments one minute after midnight
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

;Some functions to display an org-agenda list according to my workflow
(defun vedang/personal-tasklist ()
  "Display an org-agenda list of major personal tasks"
  (interactive)
  (setq tempvar org-agenda-files)
  (setq org-agenda-files (quote ((concat org-directory "/Personal-major.org") (concat org-directory "/Personal-minor.org"))))
  (org-todo-list nil)
  (setq org-agenda-files tempvar))
(global-set-key (kbd "C-c p") 'vedang/personal-tasklist)

(defun vedang/work-tasklist ()
  "Display an org-agenda list of major work tasks"
  (interactive)
  (setq tempvar org-agenda-files)
  (setq org-agenda-files (quote ((concat org-directory "/Worklog-major.org") (concat org-directory "/Worklog-minor.org") (concat org-directory "/Planner.org"))))
  (org-todo-list nil)
  (setq org-agenda-files tempvar))
(global-set-key (kbd "C-c w") 'vedang/work-tasklist)

(defun vedang/minor-tasks ()
  "Display an org-agenda list of all the pending minor tasks"
  (interactive)
  (setq tempvar org-agenda-files)
  (setq org-agenda-files (quote ((concat org-directory "/Birthdays.org") (concat org-directory "/remember-notes.org"))))
  (org-todo-list nil)
  (setq org-agenda-files tempvar))
(global-set-key (kbd "C-c m") 'vedang/minor-tasks)

; settings for Beamer
;(setq org-ditaa-jar-path "~/.emacs.d/plugins/org-mode/contrib/scripts/ditaa.jar")
;(require 'org-babel-init)
;(require 'org-babel-dot)
;(require 'org-babel-ditaa)
;(org-babel-load-library-of-babel)

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

;I don't want org mode to export a_b as a subscript b in latex.
;I mostly write code documents and this is never the intended behavior
(setq org-export-with-sub-superscripts nil)

;I want to add comments to my org files without
;having them show up in the latex export.
(setq org-export-blocks nil)

(add-to-list 'org-export-blocks
	     '(src org-babel-exp-src-blocks nil))
(add-to-list 'org-export-blocks
	     '(comment org-export-blocks-format-comment nil))
(add-to-list 'org-export-blocks
	     '(ditaa org-export-blocks-format-ditaa nil))
(add-to-list 'org-export-blocks
	     '(dot org-export-blocks-format-dot nil))

(provide 'org-mode-config)
