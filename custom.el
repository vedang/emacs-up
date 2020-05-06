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
   (quote
    ("97965ccdac20cae22c5658c282544892959dc541af3e9ef8857dbf22eb70e82b" "420459d6eeb45aadf5db5fbcc3d6990b65141c104911f7359454fc29fa9d87a0" "35ce59fe20479957989ed789edd305adac5020ed5cf6dabda4ae351d5e380520" "02591317120fb1d02f8eb4ad48831823a7926113fa9ecfb5a59742420de206e0" "170bb47b35baa3d2439f0fd26b49f4278e9a8decf611aa33a0dad1397620ddc3" "fa2af0c40576f3bde32290d7f4e7aa865eb6bf7ebe31eb9e37c32aa6f4ae8d10" default)))
 '(hl-sexp-background-color "#060404")
 '(org-agenda-files nil)
 '(package-selected-packages (quote (persist inflections queue)))
 '(safe-local-variable-values
   (quote
    ((checkdoc-package-keywords-flag)
     (eval font-lock-add-keywords nil
           (\`
            (((\,
               (concat "("
                       (regexp-opt
                        (quote
                         ("sp-do-move-op" "sp-do-move-cl" "sp-do-put-op" "sp-do-put-cl" "sp-do-del-op" "sp-do-del-cl"))
                        t)
                       "\\_>"))
              1
              (quote font-lock-variable-name-face)))))
     (eval define-clojure-indent
           (clj-action
            (quote defun))
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
           (add-hook
            (quote write-contents-functions)
            (lambda nil
              (delete-trailing-whitespace)
              nil))
           (require
            (quote whitespace))
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
           (def-collect-plan-fn
             (quote defun)))
     (eval define-clojure-indent
           (defplan
             (quote defun))
           (def-aggregate-plan-fn
             (quote defun)))
     (eval define-clojure-indent
           (cluster-spec 1)
           (group-spec 1))
     (eval define-clojure-indent
           (facts
            (quote defun))
           (fact-group
            (quote defun)))
     (lexical-binding . t)))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
