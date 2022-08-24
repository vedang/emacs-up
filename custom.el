(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(TeX-view-program-selection
   '(((output-dvi has-no-display-manager)
      "dvi2tty")
     ((output-dvi style-pstricks)
      "dvips and gv")
     (output-dvi "xdvi")
     (output-pdf "PDF Tools")
     (output-html "xdg-open")))
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["#fffefe" "#9d0000" "#006a00" "#0e1b00" "#6845138" "#840086" "#003567" "#494949"])
 '(bmkp-last-as-first-bookmark-file "~/.emacs.d/bookmarks")
 '(cider-repl-print-length 200)
 '(custom-safe-themes
   '("97965ccdac20cae22c5658c282544892959dc541af3e9ef8857dbf22eb70e82b" "420459d6eeb45aadf5db5fbcc3d6990b65141c104911f7359454fc29fa9d87a0" "35ce59fe20479957989ed789edd305adac5020ed5cf6dabda4ae351d5e380520" "02591317120fb1d02f8eb4ad48831823a7926113fa9ecfb5a59742420de206e0" "170bb47b35baa3d2439f0fd26b49f4278e9a8decf611aa33a0dad1397620ddc3" "fa2af0c40576f3bde32290d7f4e7aa865eb6bf7ebe31eb9e37c32aa6f4ae8d10" default))
 '(hl-sexp-background-color "#060404")
 '(mml-secure-openpgp-sign-with-sender t)
 '(org-agenda-files
   '("/Users/nejo/Tresors/Documents/diary/README.org" "/Users/nejo/Tresors/Documents/diary/refile.org" "/Users/nejo/Tresors/Documents/diary/tasks.org" "/Users/nejo/Tresors/Documents/diary/brain/books/art_for_money.org" "/Users/nejo/Tresors/Documents/diary/brain/books/books_index_outdated.org" "/Users/nejo/Tresors/Documents/diary/brain/books/daniel_on_mckinsey.org" "/Users/nejo/Tresors/Documents/diary/brain/books/equity_compensation.org" "/Users/nejo/Tresors/Documents/diary/brain/books/essentials_of_programming_languages.org" "/Users/nejo/Tresors/Documents/diary/brain/books/financial_intelligence.org" "/Users/nejo/Tresors/Documents/diary/brain/books/mature_optimization.org" "/Users/nejo/Tresors/Documents/diary/brain/books/onlisp.org" "/Users/nejo/Tresors/Documents/diary/brain/books/patterns_of_strategy.org" "/Users/nejo/Tresors/Documents/diary/brain/books/pgp_and_gpg.org" "/Users/nejo/Tresors/Documents/diary/brain/books/poetry.org" "/Users/nejo/Tresors/Documents/diary/brain/books/reading.org" "/Users/nejo/Tresors/Documents/diary/brain/books/seven_habits.org" "/Users/nejo/Tresors/Documents/diary/brain/books/shape_up.org" "/Users/nejo/Tresors/Documents/diary/brain/books/software_design_for_flexibility.org" "/Users/nejo/Tresors/Documents/diary/brain/books/that_will_never_work.org" "/Users/nejo/Tresors/Documents/diary/brain/books/the_pragmatic_programmer.org" "/Users/nejo/Tresors/Documents/diary/ideas/bank_to_ledger.org" "/Users/nejo/Tresors/Documents/diary/ideas/ideas_index.org" "/Users/nejo/Tresors/Documents/diary/ideas/note-taking.org" "/Users/nejo/Tresors/Documents/diary/linklog/datastructures.org" "/Users/nejo/Tresors/Documents/diary/linklog/engineering.org" "/Users/nejo/Tresors/Documents/diary/linklog/engmgmt.org" "/Users/nejo/Tresors/Documents/diary/linklog/great_reads.org" "/Users/nejo/Tresors/Documents/diary/linklog/oldlog.org" "/Users/nejo/Tresors/Documents/diary/linklog/oldpb.org" "/Users/nejo/Tresors/Documents/diary/linklog/pinboard.org" "/Users/nejo/Tresors/Documents/diary/linklog/tweets.org" "/Users/nejo/Tresors/Documents/diary/notes/fdb.org" "/Users/nejo/Tresors/Documents/diary/notes/holdings.org" "/Users/nejo/Tresors/Documents/diary/notes/life_lessons.org" "/Users/nejo/Tresors/Documents/diary/notes/tech_talks.org" "/Users/nejo/Tresors/Documents/diary/vault/2019.org" "/Users/nejo/Tresors/Documents/diary/vault/2019_december.org" "/Users/nejo/Tresors/Documents/diary/vault/2019_july.org" "/Users/nejo/Tresors/Documents/diary/vault/2020.org" "/Users/nejo/Tresors/Documents/diary/vault/2020_january.org" "/Users/nejo/Tresors/Documents/diary/brain/books/art_for_money.org" "/Users/nejo/Tresors/Documents/diary/brain/books/books_index_outdated.org" "/Users/nejo/Tresors/Documents/diary/brain/books/daniel_on_mckinsey.org" "/Users/nejo/Tresors/Documents/diary/brain/books/equity_compensation.org" "/Users/nejo/Tresors/Documents/diary/brain/books/essentials_of_programming_languages.org" "/Users/nejo/Tresors/Documents/diary/brain/books/financial_intelligence.org" "/Users/nejo/Tresors/Documents/diary/brain/books/mature_optimization.org" "/Users/nejo/Tresors/Documents/diary/brain/books/onlisp.org" "/Users/nejo/Tresors/Documents/diary/brain/books/patterns_of_strategy.org" "/Users/nejo/Tresors/Documents/diary/brain/books/pgp_and_gpg.org" "/Users/nejo/Tresors/Documents/diary/brain/books/poetry.org" "/Users/nejo/Tresors/Documents/diary/brain/books/reading.org" "/Users/nejo/Tresors/Documents/diary/brain/books/seven_habits.org" "/Users/nejo/Tresors/Documents/diary/brain/books/shape_up.org" "/Users/nejo/Tresors/Documents/diary/brain/books/software_design_for_flexibility.org" "/Users/nejo/Tresors/Documents/diary/brain/books/that_will_never_work.org" "/Users/nejo/Tresors/Documents/diary/brain/books/the_pragmatic_programmer.org" "/Users/nejo/Tresors/Documents/diary/brain/data/10/522c0c-bbeb-4a86-89c0-ef395349527d/org-mode.org" "/Users/nejo/Tresors/Documents/diary/brain/data/be/07/170aee-6bc2-4109-822d-abb4b637ac22/go-jira-screencast-script.org" "/Users/nejo/Tresors/Documents/diary/brain/data/be/07/170aee-6bc2-4109-822d-abb4b637ac22/jira-backlog.org" "/Users/nejo/Tresors/Documents/diary/brain/data/be/2F/6DA133-B1E2-4DCB-A37F-BA1207DACE22/mayank.org" "/Users/nejo/Tresors/Documents/diary/brain/data/be/38/A0615D-4F22-4925-BFBB-C0166CDD172B/gantt.org" "/Users/nejo/Tresors/Documents/diary/brain/data/be/5b/e83d98-a137-4e07-bc0c-ec84ebb1d32a/notes.org" "/Users/nejo/Tresors/Documents/diary/brain/data/be/E0/20A21B-F591-45DB-9E7A-7D12A078C7E1/parul.org" "/Users/nejo/Tresors/Documents/diary/brain/data/be/c0/3b3ba3-df82-41f2-8e54-d7b31837de86/bank_to_ledger.org" "/Users/nejo/Tresors/Documents/diary/brain/docs/levels.org" "/Users/nejo/Tresors/Documents/diary/brain/docs/okrs.org" "/Users/nejo/Tresors/Documents/diary/brain/docs/pdftools.org" "/Users/nejo/Tresors/Documents/diary/brain/Foundation.org" "/Users/nejo/Tresors/Documents/diary/brain/books.org" "/Users/nejo/Tresors/Documents/diary/brain/career.org" "/Users/nejo/Tresors/Documents/diary/brain/daily.org" "/Users/nejo/Tresors/Documents/diary/brain/engineering_management.org" "/Users/nejo/Tresors/Documents/diary/brain/finance.org" "/Users/nejo/Tresors/Documents/diary/brain/fitness.org" "/Users/nejo/Tresors/Documents/diary/brain/helpshift.org" "/Users/nejo/Tresors/Documents/diary/brain/high_performance.org" "/Users/nejo/Tresors/Documents/diary/brain/index.org" "/Users/nejo/Tresors/Documents/diary/brain/linklog.org" "/Users/nejo/Tresors/Documents/diary/brain/linklog_unread.org" "/Users/nejo/Tresors/Documents/diary/brain/mahabharata.org" "/Users/nejo/Tresors/Documents/diary/brain/marginalia.org" "/Users/nejo/Tresors/Documents/diary/brain/meetings.org" "/Users/nejo/Tresors/Documents/diary/brain/orgmode.org" "/Users/nejo/Tresors/Documents/diary/brain/presentations.org" "/Users/nejo/Tresors/Documents/diary/brain/prm.org" "/Users/nejo/Tresors/Documents/diary/brain/software_development.org" "/Users/nejo/Tresors/Documents/diary/brain/tasks.org" "/Users/nejo/Tresors/Documents/diary/brain/wellbeing.org"))
 '(org-hugo-preserve-filling nil)
 '(org-super-agenda-mode t)
 '(package-selected-packages
   '(map gnu-elpa-keyring-update xref elpher rainbow-mode jsonrpc ascii-art-to-unicode persist inflections queue))
 '(pdf-annot-default-annotation-properties
   '((t
      (label . "Vedang Manerikar")
      (color . "light green"))
     (text
      (color . "#ff0000")
      (icon . "Note"))
     (highlight
      (color . "dark cyan"))
     (underline
      (color . "blue"))
     (squiggly
      (color . "orange"))
     (strike-out
      (color . "red"))))
 '(pdf-annot-list-listed-types
   '(file free-text highlight squiggly strike-out text underline))
 '(pdf-annot-minor-mode-map-prefix [3 1])
 '(safe-local-variable-values
   '((org-taskjuggler-default-global-properties . "shift s40 \"Working Shift\" {\12   workinghours sat, sun off\12}\12leaves holiday \"New Year\" 2022-01-01\12flags hsc_z, hsc_a\12")
     (org-taskjuggler-default-global-properties . "shift s40 \"Working Shift\" {\12   workinghours sat, sun off\12}\12leaves holiday \"New Year\" 2020-01-01\12flags hsc_z, hsc_a\12")
     (org-duration-units
      ("min" . 1)
      ("h" . 60)
      ("d" . 480)
      ("w" . 2400)
      ("m" . 9600)
      ("y" . 96000))
     (org-taskjuggler-keep-project-as-task . t)
     (org-taskjuggler-target-version . 3.6)
     (org-taskjuggler-default-global-properties . "shift s40 \"Working Shift\" {\12   workinghours sat, sun off\12}\12leaves holiday \"New Year\" 2020-01-01\12")
     (checkdoc-package-keywords-flag)
     (eval font-lock-add-keywords nil
           `((,(concat "("
                       (regexp-opt
                        '("sp-do-move-op" "sp-do-move-cl" "sp-do-put-op" "sp-do-put-cl" "sp-do-del-op" "sp-do-del-cl")
                        t)
                       "\\_>")
              1 'font-lock-variable-name-face)))
     (eval define-clojure-indent
           (clj-action 'defun)
           (implement-action 4))
     (eval define-clojure-indent
           (on-one-node 1))
     (eval define-clojure-indent
           (with-action-values 1)
           (with-service-restart 1))
     (eval define-clojure-indent
           (plan-when 1)
           (plan-when-not 1))
     (eval ignore-errors "Write-contents-functions is a buffer-local alternative to before-save-hook"
           (add-hook 'write-contents-functions
                     (lambda nil
                       (delete-trailing-whitespace)
                       nil))
           (require 'whitespace)
           "Sometimes the mode needs to be toggled off and on."
           (whitespace-mode 0)
           (whitespace-mode 1))
     (whitespace-line-column . 80)
     (whitespace-style face trailing lines-tail)
     (require-final-newline . t)
     (eval define-clojure-indent
           (phase-context 2)
           (defmethod-plan 2))
     (eval define-clojure-indent
           (def-collect-plan-fn 'defun))
     (eval define-clojure-indent
           (defplan 'defun)
           (def-aggregate-plan-fn 'defun))
     (eval define-clojure-indent
           (cluster-spec 1)
           (group-spec 1))
     (eval define-clojure-indent
           (facts 'defun)
           (fact-group 'defun))
     (lexical-binding . t))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auto-dim-other-buffers-face ((t (:background "#2E3440")))))
