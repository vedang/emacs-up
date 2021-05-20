;;; init-diminish.el --- Configuration for diminish mode
;;; Author: Vedang Manerikar
;;; Created on: 05 Aug 2014
;;; Copyright (c) 2014 Vedang Manerikar <vedang.manerikar@gmail.com>

;; This file is not part of GNU Emacs.

;;; License:

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the Do What The Fuck You Want to
;; Public License, Version 2, which is included with this distribution.
;; See the file LICENSE.txt

;;; Commentary:
;; Diminish is a neat way to remove clutter from the mode-line. I
;; don't need to know that these minor modes are active, since they
;; are an integral part of my workflow and are always active.

;;; Code:

(with-eval-after-load 'flyspell
  (diminish 'flyspell-mode))
(with-eval-after-load 'paredit
  (diminish 'paredit-mode))
(with-eval-after-load 'yasnippet
  (diminish 'yas-minor-mode))
(with-eval-after-load 'whitespace
  (diminish 'whitespace-mode))
(with-eval-after-load 'smart-tab
  (diminish 'smart-tab-mode))
(with-eval-after-load 'clj-refactor
  (diminish 'clj-refactor-mode))
(with-eval-after-load 'simple
  (diminish 'visual-line-mode))
(with-eval-after-load 'subword
  (diminish 'subword-mode)
  (diminish 'superword-mode))
(with-eval-after-load 'helm-mode
  (diminish 'helm-mode))
(with-eval-after-load 'projectile
  (diminish 'projectile-mode))
(with-eval-after-load 'hideshow
  (diminish 'hs-minor-mode))
(with-eval-after-load 'noutline
  (diminish 'outline-mode))
(with-eval-after-load 'ansible-doc
  (diminish 'ansible-doc-mode))
(with-eval-after-load 'autorevert
  (diminish 'auto-revert-mode))
(with-eval-after-load 'company
  (diminish 'company-mode))

(provide 'init-dimish)
;;; init-diminish.el ends here
