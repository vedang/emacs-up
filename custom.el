(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["#fffefe" "#9d0000" "#006a00" "#0e1b00" "#6845138" "#840086" "#003567" "#494949"])
 '(cider-repl-print-length 200)
 '(custom-safe-themes
   '("97965ccdac20cae22c5658c282544892959dc541af3e9ef8857dbf22eb70e82b" "420459d6eeb45aadf5db5fbcc3d6990b65141c104911f7359454fc29fa9d87a0" "35ce59fe20479957989ed789edd305adac5020ed5cf6dabda4ae351d5e380520" "02591317120fb1d02f8eb4ad48831823a7926113fa9ecfb5a59742420de206e0" "170bb47b35baa3d2439f0fd26b49f4278e9a8decf611aa33a0dad1397620ddc3" "fa2af0c40576f3bde32290d7f4e7aa865eb6bf7ebe31eb9e37c32aa6f4ae8d10" default))
 '(hl-sexp-background-color "#060404")
 '(org-agenda-files
   '("/home/vedang/Documents/gtd/work/2020-Q1-hscore.org" "/home/vedang/Documents/gtd/work/2020-Q2.org" "/home/vedang/Documents/gtd/work/backlog.org" "/home/vedang/Documents/gtd/work/gcal.org" "/home/vedang/Documents/gtd/work/meetings.org" "/home/vedang/Documents/gtd/work/team.org" "/home/vedang/Documents/gtd/work/tjp-test.org" "/home/vedang/Tresors/Documents/diary/tasks.org" "/home/vedang/Documents/gtd/refile.org" "/home/vedang/Documents/gtd/work/brain/Automation.org" "/home/vedang/Documents/gtd/work/brain/Clojure.org" "/home/vedang/Documents/gtd/work/brain/Foundation.org" "/home/vedang/Documents/gtd/work/brain/index.org" "/home/vedang/Documents/gtd/work/brain/linklog.org" "/home/vedang/Documents/gtd/work/brain/self.org" "/home/vedang/Tresors/Documents/diary/books/financial_intelligence.org" "/home/vedang/Tresors/Documents/diary/books/index.org" "/home/vedang/Tresors/Documents/diary/books/patterns_of_strategy.org" "/home/vedang/Tresors/Documents/diary/books/reading.org" "/home/vedang/Tresors/Documents/diary/books/readlog.org" "/home/vedang/Tresors/Documents/diary/books/seven_habits.org" "/home/vedang/Tresors/Documents/diary/books/shape_up.org" "/home/vedang/Tresors/Documents/diary/books/yaksha.org" "/home/vedang/Tresors/Documents/diary/ideas/bank_to_ledger.org" "/home/vedang/Tresors/Documents/diary/ideas/index.org" "/home/vedang/Tresors/Documents/diary/ideas/note-taking.org" "/home/vedang/Tresors/Documents/diary/linklog/datastructures.org" "/home/vedang/Tresors/Documents/diary/linklog/engineering.org" "/home/vedang/Tresors/Documents/diary/linklog/engmgmt.org" "/home/vedang/Tresors/Documents/diary/linklog/great_reads.org" "/home/vedang/Tresors/Documents/diary/linklog/oldlog.org" "/home/vedang/Tresors/Documents/diary/linklog/oldpb.org" "/home/vedang/Tresors/Documents/diary/linklog/pinboard.org" "/home/vedang/Tresors/Documents/diary/linklog/tweets.org" "/home/vedang/Tresors/Documents/diary/notes/fdb.org" "/home/vedang/Tresors/Documents/diary/notes/holdings.org" "/home/vedang/Tresors/Documents/diary/notes/life_lessons.org" "/home/vedang/Tresors/Documents/diary/notes/tech_talks.org" "/home/vedang/Tresors/Documents/diary/vault/2019.org" "/home/vedang/Tresors/Documents/diary/vault/2019_december.org" "/home/vedang/Tresors/Documents/diary/vault/2019_july.org" "/home/vedang/Tresors/Documents/diary/vault/2020.org" "/home/vedang/Tresors/Documents/diary/vault/2020_january.org" "/home/vedang/Tresors/Documents/diary/brain/career.org" "/home/vedang/Tresors/Documents/diary/brain/engineering_management.org" "/home/vedang/Tresors/Documents/diary/brain/errands.org" "/home/vedang/Tresors/Documents/diary/brain/finance.org" "/home/vedang/Tresors/Documents/diary/brain/fitness.org" "/home/vedang/Tresors/Documents/diary/brain/index.org" "/home/vedang/Tresors/Documents/diary/brain/linklog.org" "/home/vedang/Tresors/Documents/diary/brain/linklog_unread.org" "/home/vedang/Tresors/Documents/diary/brain/orgmode.org" "/home/vedang/Tresors/Documents/diary/brain/presentations.org" "/home/vedang/Tresors/Documents/diary/brain/prm.org" "/home/vedang/Tresors/Documents/diary/brain/software_development.org" "/home/vedang/Tresors/Documents/diary/brain/wellbeing.org"))
 '(org-super-agenda-mode t)
 '(package-selected-packages '(ascii-art-to-unicode persist inflections queue))
 '(safe-local-variable-values
   '((checkdoc-package-keywords-flag)
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
 )
