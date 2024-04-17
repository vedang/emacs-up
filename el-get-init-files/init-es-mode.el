;;; init-es-mode.el --- Configuration for es-mode -*- lexical-binding: t -*-
;;; Commentary:
;;; Author: Vedang Manerikar
;;; Created on: 9 Jul 2016
;;; Copyright (c) 2016 Vedang Manerikar <vedang.manerikar@gmail.com>

;; This file is not part of GNU Emacs.

;;; License:

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the Do What The Fuck You Want to
;; Public License, Version 2, which is included with this distribution.
;; See the file LICENSE.txt

;;; Code:

(add-to-list 'auto-mode-alist '("\\.es$" . es-mode))
(setq es-always-pretty-print t)
(with-eval-after-load 'es-cc
  (setq es-cc-endpoint "http://shiva.local:9202"))

(provide 'init-es-mode)
;;; init-es-mode.el ends here
