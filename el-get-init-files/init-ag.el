;;; init-ag.el --- Configuration for ag mode
;;; Commentary:
;;; Author: Vedang Manerikar
;;; Created on: 3 May 2016
;;; Copyright (c) 2016 Vedang Manerikar <vedang.manerikar@gmail.com>

;; This file is not part of GNU Emacs.

;;; License:

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the Do What The Fuck You Want to
;; Public License, Version 2, which is included with this distribution.
;; See the file LICENSE.txt

;;; Code:

(setq ag-highlight-search t)
(add-hook 'ag-mode-hook 'next-error-follow-minor-mode)

(provide 'init-ag)
;;; init-ag.el ends here
