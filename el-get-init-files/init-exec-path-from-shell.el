;;; init-exec-path-from-shell.el - Manage paths correctly on OSX and
;;; other non-conformist systems
;;; Author: Vedang Manerikar
;;; Created on: 08 Aug 2014
;;; Copyright (c) 2014 Vedang Manerikar <vedang.manerikar@gmail.com>

;; This file is not part of GNU Emacs.

;;; License:

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the Do What The Fuck You Want to
;; Public License, Version 2, which is included with this distribution.
;; See the file LICENSE.txt

;;; Code:
 (progn (exec-path-from-shell-initialize)
        (exec-path-from-shell-copy-env "PYTHONPATH")
        (exec-path-from-shell-copy-env "GOPATH"))

(provide 'init-exec-path-from-shell)
