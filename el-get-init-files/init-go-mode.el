;;; init-go-mode.el --- configuration for go-mode:
;;; Author: Vedang Manerikar
;;; Created on: 8 Aug 2014
;;; Copyright (c) 2014 Vedang Manerikar <vedang.manerikar@gmail.com>

;; This file is not part of GNU Emacs.

;;; License:

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the Do What The Fuck You Want to
;; Public License, Version 2, which is included with this distribution.
;; See the file LICENSE.txt

;;; Code:

(with-eval-after-load 'go-mode
  (add-hook 'before-save-hook 'gofmt-before-save)

  ;; Add functions to jump back. This will be merged into
  ;; go-mode master soon hopefully.
  (defvar godef--marker-list nil)

  (defun godef-jump (point &optional other-window)
    "Jump to the definition of the expression at POINT."
    (interactive "d")
    (condition-case nil
        (let ((file (car (godef--call point))))
          (if (not (godef--successful-p file))
              (message "%s" (godef--error file))
            (push (point-marker) godef--marker-list)
            (ring-insert find-tag-marker-ring (point-marker))
            (godef--find-file-line-column file other-window)))
      (file-error (message "Could not run godef binary"))))

  (defun godef-jump-back ()
    "Pop back to where `godef-jump' was last invoked"
    (interactive)
    (when (null godef--marker-list)
      (error "Marker list is empty. Can't pop back"))
    (let ((marker (pop godef--marker-list)))
      (switch-to-buffer (or (marker-buffer marker)
                            (error "Buffer has been deleted")))
      (goto-char (marker-position marker))
      ;; Cleanup the marker so as to avoid them piling up.
      (set-marker marker nil nil)))

  (define-key go-mode-map (kbd "M-.") 'godef-jump)
  (define-key go-mode-map (kbd "M-,") 'godef-jump-back))


(provide 'init-go-mode)
