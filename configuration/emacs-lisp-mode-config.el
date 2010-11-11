(defun compile-el-on-save ()
  "If saving an elisp file, byte-compile it."
  (add-hook 'after-save-hook 'emacs-lisp-byte-compile t t))

(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (compile-el-on-save)))
;===============================================================================

(defun rgr/toggle-context-help()
  "Turn on or off the context help.
Note that if ON and you hide the help buffer then you need to
manually reshow it. A double toggle will make it reappear"
  (interactive)
  (with-current-buffer (help-buffer)
    (unless (local-variable-p 'context-help)
      (set (make-local-variable 'context-help) t))
    (if (setq context-help (not context-help))
        (progn
          (if (not (get-buffer-window (help-buffer)))
              (display-buffer (help-buffer)))))
    (message "Context help %s" (if context-help "ON" "OFF"))))

(defun rgr/context-help()
  "Display function or variable at point in *Help* buffer if visible.
Default behaviour can be turned off by setting the buffer local
context-help to false"
  (interactive)

  (let(( rgr-symbol (symbol-at-point))) ; symbol-at-point http://www.emacswiki.org/cgi-bin/wiki/thingatpt%2B.el
    (with-current-buffer (help-buffer)
      (unless (local-variable-p 'context-help)
        (set (make-local-variable 'context-help) t))
      (if (and context-help (get-buffer-window (help-buffer))
               rgr-symbol)
          (if (fboundp  rgr-symbol)
              (describe-function rgr-symbol)
            (if (boundp  rgr-symbol) (describe-variable rgr-symbol)))))))
;===============================================================================

(defadvice eldoc-print-current-symbol-info
  (around eldoc-show-c-tag activate)
  (cond
   ((eq major-mode 'emacs-lisp-mode)(rgr/context-help) ad-do-it)
   ((eq major-mode 'lisp-interaction-mode)(rgr/context-help) ad-do-it)
   ((eq major-mode 'apropos-mode)(rgr/context-help) ad-do-it)
   (t ad-do-it)))

(global-set-key (kbd "C-c h") 'rgr/toggle-context-help)
;===============================================================================

(provide 'emacs-lisp-mode-config)
