;;; init-geiser.el --- Configuration for Geiser
;;; Author: Vedang Manerikar
;;; Created on: 06 Jan 2015
;;; Copyright (c) 2015 Vedang Manerikar <vedang.manerikar@gmail.com>

;; This file is not part of GNU Emacs.

;;; License:

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the Do What The Fuck You Want to
;; Public License, Version 2, which is included with this distribution.
;; See the file LICENSE.txt

;;; Code:

(add-to-list 'auto-mode-alist
             '("\\.scm\\'" . scheme-mode))

(setq geiser-repl-history-filename (concat tempfiles-dirname "geiser-history"))

(provide 'init-geiser)

;;; init-geiser ends here
