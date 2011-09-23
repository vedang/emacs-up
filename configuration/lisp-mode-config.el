;;; Slime

(setq slime-net-coding-system 'utf-8-unix)

(slime-setup '(slime-fancy
               slime-repl
               slime-asdf
               slime-fuzzy))

(add-hook 'lisp-mode-hook 'turn-on-slime)
(add-hook 'inferior-lisp-mode-hook 'turn-on-slime)
(add-hook 'slime-mode-hook 'turn-on-paredit)
(add-hook 'slime-repl-mode-hook 'turn-on-paredit)
(add-hook 'slime-connected-hook 'turn-on-paredit)


(defadvice slime-repl-emit (after sr-emit-ad activate)
  (with-current-buffer (slime-output-buffer)
    (add-text-properties slime-output-start slime-output-end
                         '(font-lock-face slime-repl-output-face
                                          rear-nonsticky (font-lock-face)))))

(defadvice slime-repl-insert-prompt (after sr-prompt-ad activate)
  (with-current-buffer (slime-output-buffer)
    (let ((inhibit-read-only t))
      (add-text-properties slime-repl-prompt-start-mark (point-max)
                           '(font-lock-face slime-repl-prompt-face
                                            rear-nonsticky
                                            (slime-repl-prompt
                                             read-only
                                             font-lock-face
                                             intangible))))))

;;; Auto complete integration with slime
(require 'ac-slime)
(add-hook 'slime-mode-hook 'set-up-slime-ac)


(provide 'lisp-mode-config)
