;;; init-helm-projectile.el --- Configuration for the helm-projectile package
;;; Commentary:
;;; Author: Vedang Manerikar
;;; Created on: 22 Oct 2016
;;; Copyright (c) 2016 Vedang Manerikar <vedang.manerikar@gmail.com>

;; This file is not part of GNU Emacs.

;;; License:

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the Do What The Fuck You Want to
;; Public License, Version 2, which is included with this distribution.
;; See the file LICENSE.txt

;;; Code:

(require 'helm-projectile)

(projectile-mode)

(setq projectile-completion-system 'helm
      projectile-switch-project-action 'helm-projectile
      projectile-enable-caching t)

(helm-projectile-on)

(provide 'init-helm-projectile)
;;; init-helm-projectile.el ends here
