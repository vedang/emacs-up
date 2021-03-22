;;; init-diminish.el - Configuration for diminish mode
;;; Author: Vedang Manerikar
;;; Created on: 05 Aug 2014
;;; Copyright (c) 2014 Vedang Manerikar <vedang.manerikar@gmail.com>

;; This file is not part of GNU Emacs.

;;; License:

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the Do What The Fuck You Want to
;; Public License, Version 2, which is included with this distribution.
;; See the file LICENSE.txt

;;; Code:;;;

(eval-after-load 'flyspell
  '(diminish 'flyspell-mode))
(eval-after-load 'paredit
  '(diminish 'paredit-mode "P"))
(eval-after-load 'yasnippet
  '(diminish 'yas-minor-mode))
(eval-after-load 'whitespace
  '(diminish 'whitespace-mode))
(eval-after-load 'smart-tab
  '(diminish 'smart-tab-mode))
(eval-after-load 'clj-refactor
  '(diminish 'clj-refactor-mode))
(eval-after-load 'simple
  '(diminish 'visual-line-mode))
(eval-after-load 'subword
  '(diminish 'subword-mode))
(eval-after-load 'helm-mode
  '(diminish 'helm-mode))
