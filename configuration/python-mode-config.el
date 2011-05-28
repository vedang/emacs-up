;;; settings for python

(require 'pymacs)
(pymacs-load "ropemacs" "rope-")
(setq ropemacs-confirm-saving 'nil
      ropemacs-enable-autoimport t)

;;; ropemacs Integration with auto-completion
;;; from github.com/gabrielelanaro
(defun ac-ropemacs-candidates ()
  (mapcar (lambda (completion)
            (concat ac-prefix completion))
          (rope-completions)))

(ac-define-source nropemacs
  '((candidates . ac-ropemacs-candidates)
    (symbol . "p")))

(ac-define-source nropemacs-dot
  '((candidates . ac-ropemacs-candidates)
    (symbol . "p")
    (prefix . c-dot)
    (requires . 0)))

(defun ac-nropemacs-setup ()
  (setq ac-sources (append '(ac-source-nropemacs
                             ac-source-nropemacs-dot) ac-sources)))
(defun ac-python-mode-setup ()
  (add-to-list 'ac-sources 'ac-source-yasnippet))

(add-hook 'python-mode-hook 'ac-python-mode-setup)
(add-hook 'rope-open-project-hook 'ac-nropemacs-setup)

(provide 'python-mode-config)
