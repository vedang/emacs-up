;;cscope settings
;===============================================================================

(setq cscope-do-not-update-database t)

(define-key global-map [(control f3)]  'cscope-set-initial-directory)
(define-key global-map [(control f4)]  'cscope-find-this-file)
(define-key global-map [(control f5)]  'cscope-find-this-symbol)
(define-key global-map [(control f6)]  'cscope-find-global-definition)
(define-key global-map [(control f7)]  'cscope-find-this-text-string)
(define-key global-map [(control f8)]  'cscope-pop-mark)
(define-key global-map [(control f9)]  'cscope-find-functions-calling-this-function)
(define-key global-map [(control f10)] 'cscope-find-called-functions)
(define-key global-map [(control f11)] 'cscope-display-buffer)
;===============================================================================

(provide 'cscope-mode-config)

