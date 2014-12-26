;;; features.el - What do you want to install?
;;; Author: Vedang Manerikar
;;; Created on: 22 Aug 2014
;;; Copyright (c) 2014 Vedang Manerikar <vedang.manerikar@gmail.com>

;; This file is not part of GNU Emacs.

;;; License:

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the Do What The Fuck You Want to
;; Public License, Version 2, which is included with this distribution.
;; See the file LICENSE.txt

;;; Code:


;; Mark features you need as `t', and ones you don't need as `nil'
;; Eg: if you program in clojure, you should set `configure-clojure-p' to `t'


(setq configure-clojure-p nil
      configure-go-p nil
      configure-python-p nil)
