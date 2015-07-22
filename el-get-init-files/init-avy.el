;;; init-avy.el --- Configuration for Avy
;;; Author: Vedang Manerikar
;;; Created on: 22 July 2015
;;; Copyright (c) 2015 Vedang Manerikar <vedang.manerikar@gmail.com>

;; This file is not part of GNU Emacs.

;;; License:

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the Do What The Fuck You Want to
;; Public License, Version 2, which is included with this distribution.
;; See the file LICENSE.txt

;;; Code:

(global-set-key (kbd "M-g g") 'avy-goto-line)
(global-set-key (kbd "M-g w") 'avy-goto-word-1)
(global-set-key (kbd "M-g SPC") 'avy-goto-word-1)
(avy-setup-default)

(provide 'init-avy)
