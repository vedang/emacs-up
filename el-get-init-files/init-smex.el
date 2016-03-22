;;; init-smex.el - Configuration for Smex mode
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


(smex-initialize)
;; Moving Smex to M-c while I try out `helm'
(global-set-key (kbd "M-c") 'smex)
(global-set-key (kbd "M-C") 'smex-major-mode-commands)


(provide 'init-smex)
