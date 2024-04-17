;;; init-ac-nrepl.el - Configuration for AC-nrepl
;;; Author: Vedang Manerikar
;;; Created on: 27 Oct 2013
;;; Copyright (c) 2013 Vedang Manerikar <vedang.manerikar@gmail.com>

;; This file is not part of GNU Emacs.

;;; License:

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the Do What The Fuck You Want to
;; Public License, Version 2, which is included with this distribution.
;; See the file LICENSE.txt

;;; Code:

(defun ian/activate-ac-nrepl? ()
  "ac-nrepl doesn't work well in production environments. Don't
activate it there."
  (when (y-or-n-p "Activate AC-nREPL?")
    (ac-nrepl-setup)))


(with-eval-after-load 'cider
  (add-hook 'cider-repl-mode-hook 'ian/activate-ac-nrepl?)
  (add-hook 'cider-mode-hook 'ac-nrepl-setup))


(provide 'init-ac-nrepl)
