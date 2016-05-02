;;; init-magit.el - Configuration for magit mode
;;; Author: Vedang Manerikar
;;; Created on: 21 Oct 2013
;;; Copyright (c) 2013 Vedang Manerikar <vedang.manerikar@gmail.com>

;; This file is not part of GNU Emacs.

;;; License:

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the Do What The Fuck You Want to
;; Public License, Version 2, which is included with this distribution.
;; See the file LICENSE.txt

;;; Code:;;;


(global-set-key (kbd "C-x g") 'magit-status)

(eval-after-load 'info
  '(progn (info-initialize)
          (add-to-list 'Info-directory-list
                       (concat el-get-dir "magit/"))))

(eval-after-load 'magit
  '(progn (setq-default magit-auto-revert-mode-lighter nil)))

(provide 'init-magit)
