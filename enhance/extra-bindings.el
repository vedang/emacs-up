;;; extra-bindings.el - convenience bindings for various things.
;;; Author: Vedang Manerikar
;;; Created on: 13 Oct 2013
;;; Copyright (c) 2013 Vedang Manerikar <vedang.manerikar@gmail.com>

;; This file is not part of GNU Emacs.

;;; License:

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the Do What The Fuck You Want to
;; Public License, Version 2, which is included with this distribution.
;; See the file LICENSE.txt

;;; Code:

(require 'utility-functions)
(global-set-key (kbd "C-w") 'uf/backward-kill-word-or-kill-region)
(global-set-key (kbd "M-8") 'uf/extend-selection)
(global-set-key (kbd "C-x M-s") 'uf/transpose-windows)

(require 'presenting)
(global-set-key (kbd "<f7>") 'pr/jump-to-prev-slide)
(global-set-key (kbd "<f8>") 'pr/jump-to-next-slide)

(provide 'extra-bindings)
