;;; Configuration for ido-mode

(ido-mode t)

(setq ido-enable-prefix nil
      ido-enable-flex-matching t
      ido-create-new-buffer 'always
      ido-use-filename-at-point 'guess
      ido-max-prospects 10)

(ido-everywhere t)
(global-set-key (kbd "C-x M-f") 'ido-find-file-other-window)

;; This tab override shouldn't be necessary given ido's default
;; configuration, but minibuffer-complete otherwise dominates the
;; tab binding because of my custom tab-completion-everywhere
;; configuration. - from M-x All things emacs
(add-hook 'ido-setup-hook
          (lambda ()
            (define-key ido-completion-map [tab] 'ido-complete)))

;;; ido on steroids :D from EmacsWiki
(defadvice completing-read
  (around foo activate)
  (if (boundp 'ido-cur-list)
      ad-do-it
    (setq ad-return-value
          (ido-completing-read
           prompt
           (all-completions "" collection predicate)
           nil require-match initial-input hist def))))

(global-set-key
 "\M-x"
 (lambda ()
   (interactive)
   (call-interactively
    (intern
     (ido-completing-read
      "M-x "
      (all-completions "" obarray 'commandp))))))

(provide 'ido-mode-config)
