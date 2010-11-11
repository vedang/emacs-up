;; Settings for LaTeX mode
; ==============================================================================

(defun latex-mode-settings ()
  "my settings for LaTeX mode"
  ;always have reftex along with auctex
  (turn-on-reftex)
  (flyspell-mode))

(add-hook 'LaTeX-mode-hook 'latex-mode-settings)

(setq reftex-plug-into-AUCTeX t)
(setq TeX-auto-save t
      TeX-save-query nil
      TeX-parse-self t)
; ==============================================================================

(provide 'latex-mode-config)
