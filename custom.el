(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(safe-local-variable-values (quote ((eval define-clojure-indent (on-one-node 1)) (eval define-clojure-indent (with-action-values 1) (with-service-restart 1)) (eval define-clojure-indent (plan-when 1) (plan-when-not 1)) (eval ignore-errors "Write-contents-functions is a buffer-local alternative to before-save-hook" (add-hook (quote write-contents-functions) (lambda nil (delete-trailing-whitespace) nil)) (require (quote whitespace)) "Sometimes the mode needs to be toggled off and on." (whitespace-mode 0) (whitespace-mode 1)) (whitespace-line-column . 80) (whitespace-style face trailing lines-tail) (require-final-newline . t) (eval define-clojure-indent (phase-context 2) (defmethod-plan 2)) (eval define-clojure-indent (def-collect-plan-fn (quote defun))) (eval define-clojure-indent (defplan (quote defun)) (def-aggregate-plan-fn (quote defun))) (eval define-clojure-indent (cluster-spec 1) (group-spec 1)) (lexical-binding . t)))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
