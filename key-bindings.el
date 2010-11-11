;I need these key combos
;===============================================================================

(global-set-key (kbd "M-j") 'pop-to-mark-command)
(global-set-key (kbd "M-q") 'revert-buffer)
(global-set-key (kbd "M-e") 'bury-buffer)
(global-set-key (kbd "RET") 'newline-and-indent)
(global-set-key (kbd "C-x \\") 'align-regexp)
(global-set-key (kbd "M-d") 'backward-delete-char)
(global-set-key (kbd "M-/") 'hippie-expand)
(global-set-key (kbd "M-g") 'goto-line)
(global-set-key (kbd "C-x n r") 'narrow-to-region)
;===============================================================================

;;Keybindings from Steve Yegge's Effective Emacs
(global-set-key (kbd "C-x C-m") 'execute-extended-command) ;; faster M-x
(global-set-key (kbd "C-c C-m") 'execute-extended-command)
(global-set-key (kbd "C-w") 'backward-kill-word)           ;; easy editing
(global-set-key (kbd "C-x C-k") 'kill-region)              ;; remapping C-w
;===============================================================================

(provide 'key-bindings)
