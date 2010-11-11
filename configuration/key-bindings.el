;; I need these key combos
;; ===============================================================================

(global-set-key (kbd "M-j") 'pop-to-mark-command)
(global-set-key (kbd "RET") 'reindent-then-newline-and-indent)
(global-set-key (kbd "M-g") 'goto-line)
(global-set-key (kbd "C-x n r") 'narrow-to-region)

;; Keybindings from Steve Yegge's Effective Emacs
(global-set-key (kbd "C-x C-m") 'execute-extended-command) ;; faster M-x
(global-set-key (kbd "C-c C-m") 'execute-extended-command)
(global-set-key (kbd "C-w") 'backward-kill-word)           ;; easy editing
(global-set-key (kbd "C-x C-k") 'kill-region)              ;; remapping C-w

;; Local keybindings
(eval-after-load 'paredit
  ;; I don't need open-line and this is much easier than actually
  ;; typing Shift+9
  '(define-key paredit-mode-map (kbd "C-o") 'paredit-open-round))

;; ===============================================================================

(provide 'key-bindings)
