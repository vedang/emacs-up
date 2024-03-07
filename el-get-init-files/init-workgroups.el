;;; init-workgroups.el --- Configuration for workgroups
;;; Author: Vedang Manerikar
;;; Created on: 6 Dec 2013
;;; Copyright (c) 2013 Vedang Manerikar <vedang.manerikar@gmail.com>

;; This file is not part of GNU Emacs.

;;; License:

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the Do What The Fuck You Want to
;; Public License, Version 2, which is included with this distribution.
;; See the file LICENSE.txt

;;; Code:


(setq wg-morph-on nil
      wg-switch-on-load nil)

(workgroups-mode 1)
(wg-load wg-file)

(provide 'init-workgroups)

;;; init-workgroups ends here
