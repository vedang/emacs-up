;;; init-yasnippet.el - Configuration for yasnippet mode
;;; Author: Vedang Manerikar
;;; Created on: 10 Nov 2013
;;; Copyright (c) 2013 Vedang Manerikar <vedang.manerikar@gmail.com>

;; This file is not part of GNU Emacs.

;;; License:

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the Do What The Fuck You Want to
;; Public License, Version 2, which is included with this distribution.
;; See the file LICENSE.txt

;;; Code:


(add-to-list 'yas-snippet-dirs (concat enhance-dirname "snippets"))
(yas-global-mode 1)
