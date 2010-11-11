;; More Configuration for ido-mode
;; =============================================================================

(ido-everywhere t)

;; =============================================================================

;; This tab override shouldn't be necessary given ido's default
;; configuration, but minibuffer-complete otherwise dominates the
;; tab binding because of my custom tab-completion-everywhere
;; configuration. - from M-x All things emacs
(add-hook 'ido-setup-hook
          (lambda ()
            (define-key ido-completion-map [tab] 'ido-complete)))
;; =============================================================================

;; ido on steroids :D from EmacsWiki
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

;; =============================================================================
(provide 'ido-mode-config)
