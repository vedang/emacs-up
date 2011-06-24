;;; I need these key combos

(global-set-key (kbd "M-j") 'pop-to-mark-command)
(global-set-key (kbd "RET") 'reindent-then-newline-and-indent)
(global-set-key (kbd "M-g") 'goto-line)
(global-set-key (kbd "C-x n r") 'narrow-to-region)
(global-set-key (kbd "C-x \\") 'align-regexp)
(global-set-key (kbd "M-/") 'hippie-expand)
(global-set-key (kbd "C-c n") 'cleanup-buffer)
(global-set-key (kbd "C-c y") 'bury-buffer)
(global-set-key (kbd "C-c r") 'revert-buffer)
(global-set-key (kbd "<f5>") 'revert-buffer)
(global-set-key (kbd "C-x m") 'eshell)
(global-set-key (kbd "C-x M") (lambda () (interactive) (eshell t)))
(global-set-key (kbd "C-h a") 'apropos)
(global-set-key (kbd "C-x g") 'magit-status)
(define-key global-map [(control f3)]  'cscope-set-initial-directory)
(global-set-key (kbd "C-c a") 'org-agenda)
(define-key global-map (kbd "C-+") 'text-scale-increase)
(define-key global-map (kbd "C--") 'text-scale-decrease)


;;; Window switching. (C-x o goes to the next window)
;;; Emacs-starter-kit
(windmove-default-keybindings) ;; Shift+direction
(global-set-key (kbd "C-x O") (lambda () (interactive) (other-window -1))) ;; back one
(global-set-key (kbd "C-x C-o") (lambda () (interactive) (other-window 2))) ;; forward two


;;; Keybindings from Steve Yegge's Effective Emacs
(global-set-key (kbd "C-x C-m") 'execute-extended-command) ;; faster M-x
(global-set-key (kbd "C-c C-m") 'execute-extended-command)


;; Local keybindings
(eval-after-load 'paredit
  ;; I don't need open-line and this is much easier than actually
  ;; typing Shift+9
  '(define-key paredit-mode-map (kbd "C-o") 'paredit-open-round))


;; Aliases for common functions
(defalias 'qrr 'query-replace-regexp)
(defalias 'rvt 'revert-buffer)
(defalias 'dtw 'delete-trailing-whitespace)


(provide 'key-bindings)
