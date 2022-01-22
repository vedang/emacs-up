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
   '("/home/vedang/Tresors/Documents/gtd/work/2020-Q1-hscore.org" "/home/vedang/Tresors/Documents/gtd/work/2020-Q2.org" "/home/vedang/Tresors/Documents/gtd/work/backlog.org" "/home/vedang/Tresors/Documents/gtd/work/gcal.org" "/home/vedang/Tresors/Documents/gtd/work/team.org" "/home/vedang/Tresors/Documents/diary/refile.org" "/home/vedang/Tresors/Documents/diary/tasks.org" "/home/vedang/Tresors/Documents/diary/refile.org" "/home/vedang/Tresors/Documents/gtd/work/brain/daily.org" "/home/vedang/Tresors/Documents/gtd/work/brain/index.org" "/home/vedang/Tresors/Documents/gtd/work/brain/self.org" "/home/vedang/Tresors/Documents/diary/brain/books/art_for_money.org" "/home/vedang/Tresors/Documents/diary/brain/books/daniel_on_mckinsey.org" "/home/vedang/Tresors/Documents/diary/brain/books/equity_compensation.org" "/home/vedang/Tresors/Documents/diary/brain/books/essentials_of_programming_languages.org" "/home/vedang/Tresors/Documents/diary/brain/books/financial_intelligence.org" "/home/vedang/Tresors/Documents/diary/brain/books/index.org" "/home/vedang/Tresors/Documents/diary/brain/books/mature_optimization.org" "/home/vedang/Tresors/Documents/diary/brain/books/patterns_of_strategy.org" "/home/vedang/Tresors/Documents/diary/brain/books/poetry.org" "/home/vedang/Tresors/Documents/diary/brain/books/reading.org" "/home/vedang/Tresors/Documents/diary/brain/books/seven_habits.org" "/home/vedang/Tresors/Documents/diary/brain/books/shape_up.org" "/home/vedang/Tresors/Documents/diary/brain/books/the_pragmatic_programmer.org" "/home/vedang/Tresors/Documents/diary/ideas/bank_to_ledger.org" "/home/vedang/Tresors/Documents/diary/ideas/index.org" "/home/vedang/Tresors/Documents/diary/ideas/note-taking.org" "/home/vedang/Tresors/Documents/diary/linklog/datastructures.org" "/home/vedang/Tresors/Documents/diary/linklog/engineering.org" "/home/vedang/Tresors/Documents/diary/linklog/engmgmt.org" "/home/vedang/Tresors/Documents/diary/linklog/great_reads.org" "/home/vedang/Tresors/Documents/diary/linklog/oldlog.org" "/home/vedang/Tresors/Documents/diary/linklog/oldpb.org" "/home/vedang/Tresors/Documents/diary/linklog/pinboard.org" "/home/vedang/Tresors/Documents/diary/linklog/tweets.org" "/home/vedang/Tresors/Documents/diary/notes/fdb.org" "/home/vedang/Tresors/Documents/diary/notes/holdings.org" "/home/vedang/Tresors/Documents/diary/notes/life_lessons.org" "/home/vedang/Tresors/Documents/diary/notes/tech_talks.org" "/home/vedang/Tresors/Documents/diary/vault/2019.org" "/home/vedang/Tresors/Documents/diary/vault/2019_december.org" "/home/vedang/Tresors/Documents/diary/vault/2019_july.org" "/home/vedang/Tresors/Documents/diary/vault/2020.org" "/home/vedang/Tresors/Documents/diary/vault/2020_january.org" "/home/vedang/Tresors/Documents/diary/brain/books/art_for_money.org" "/home/vedang/Tresors/Documents/diary/brain/books/daniel_on_mckinsey.org" "/home/vedang/Tresors/Documents/diary/brain/books/equity_compensation.org" "/home/vedang/Tresors/Documents/diary/brain/books/essentials_of_programming_languages.org" "/home/vedang/Tresors/Documents/diary/brain/books/financial_intelligence.org" "/home/vedang/Tresors/Documents/diary/brain/books/index.org" "/home/vedang/Tresors/Documents/diary/brain/books/mature_optimization.org" "/home/vedang/Tresors/Documents/diary/brain/books/patterns_of_strategy.org" "/home/vedang/Tresors/Documents/diary/brain/books/poetry.org" "/home/vedang/Tresors/Documents/diary/brain/books/reading.org" "/home/vedang/Tresors/Documents/diary/brain/books/seven_habits.org" "/home/vedang/Tresors/Documents/diary/brain/books/shape_up.org" "/home/vedang/Tresors/Documents/diary/brain/books/the_pragmatic_programmer.org" "/home/vedang/Tresors/Documents/diary/brain/data/10/522c0c-bbeb-4a86-89c0-ef395349527d/org-mode.org" "/home/vedang/Tresors/Documents/diary/brain/data/be/07/170aee-6bc2-4109-822d-abb4b637ac22/go-jira-screencast-script.org" "/home/vedang/Tresors/Documents/diary/brain/data/be/07/170aee-6bc2-4109-822d-abb4b637ac22/jira-backlog.org" "/home/vedang/Tresors/Documents/diary/brain/data/be/2F/6DA133-B1E2-4DCB-A37F-BA1207DACE22/mayank.org" "/home/vedang/Tresors/Documents/diary/brain/data/be/38/A0615D-4F22-4925-BFBB-C0166CDD172B/gantt.org" "/home/vedang/Tresors/Documents/diary/brain/data/be/5b/e83d98-a137-4e07-bc0c-ec84ebb1d32a/notes.org" "/home/vedang/Tresors/Documents/diary/brain/data/be/E0/20A21B-F591-45DB-9E7A-7D12A078C7E1/parul.org" "/home/vedang/Tresors/Documents/diary/brain/data/be/c0/3b3ba3-df82-41f2-8e54-d7b31837de86/bank_to_ledger.org" "/home/vedang/Tresors/Documents/diary/brain/docs/levels.org" "/home/vedang/Tresors/Documents/diary/brain/docs/pdftools.org" "/home/vedang/Tresors/Documents/diary/brain/Foundation.org" "/home/vedang/Tresors/Documents/diary/brain/books.org" "/home/vedang/Tresors/Documents/diary/brain/career.org" "/home/vedang/Tresors/Documents/diary/brain/engineering_management.org" "/home/vedang/Tresors/Documents/diary/brain/finance.org" "/home/vedang/Tresors/Documents/diary/brain/fitness.org" "/home/vedang/Tresors/Documents/diary/brain/high_performance.org" "/home/vedang/Tresors/Documents/diary/brain/index.org" "/home/vedang/Tresors/Documents/diary/brain/linklog.org" "/home/vedang/Tresors/Documents/diary/brain/linklog_unread.org" "/home/vedang/Tresors/Documents/diary/brain/mahabharata.org" "/home/vedang/Tresors/Documents/diary/brain/meetings.org" "/home/vedang/Tresors/Documents/diary/brain/orgmode.org" "/home/vedang/Tresors/Documents/diary/brain/presentations.org" "/home/vedang/Tresors/Documents/diary/brain/prm.org" "/home/vedang/Tresors/Documents/diary/brain/software_development.org" "/home/vedang/Tresors/Documents/diary/brain/tasks.org" "/home/vedang/Tresors/Documents/diary/brain/wellbeing.org"))
 '(org-super-agenda-mode t)
 '(package-selected-packages
   '(xref elpher rainbow-mode jsonrpc ascii-art-to-unicode persist inflections queue))
 '(safe-local-variable-values
   '((org-taskjuggler-default-global-properties . "shift s40 \"Working Shift\" {
   workinghours sat, sun off
}
leaves holiday \"New Year\" 2020-01-01
flags hsc_z, hsc_a
")
     (org-duration-units
      ("min" . 1)
      ("h" . 60)
      ("d" . 480)
      ("w" . 2400)
      ("m" . 9600)
      ("y" . 96000))
     (org-taskjuggler-keep-project-as-task . t)
     (org-taskjuggler-target-version . 3.6)
     (org-taskjuggler-default-global-properties . "shift s40 \"Working Shift\" {
   workinghours sat, sun off
}
leaves holiday \"New Year\" 2020-01-01
")
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
