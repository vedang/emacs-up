;; Configuraion for perl-modes

;; Always use cperl instead of perl
(add-to-list 'auto-mode-alist '("\\.\\([pP][Llm]\\|al\\)\\'" . cperl-mode))
(add-to-list 'interpreter-mode-alist '("perl" . cperl-mode))
(add-to-list 'interpreter-mode-alist '("perl5" . cperl-mode))
(add-to-list 'interpreter-mode-alist '("miniperl" . cperl-mode))

(defun perl-compile ()
  "Run perl -c against the current file"
  (interactive)
  (shell-command (concat "perl -c " (buffer-file-name))))

(add-hook 'cperl-mode-hook '(local-set-key [f5] 'perl-compile))
(setq cperl-hairy t)
(global-set-key (kbd "C-h P") 'perldoc)
; ==============================================================================

(provide 'perl-mode-config)
