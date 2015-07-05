;;; init-ledger-mode.el --- Configuration for Ledger Mode
;;; Author: Vedang Manerikar
;;; Created on: 06 July 2015
;;; Copyright (c) 2015 Vedang Manerikar <vedang.manerikar@gmail.com>

;; This file is not part of GNU Emacs.

;;; License:

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the Do What The Fuck You Want to
;; Public License, Version 2, which is included with this distribution.
;; See the file LICENSE.txt

;;; Code:

(setq ledger-reconcile-default-commodity "Rs"
      ledger-reports
      '(("balablc" "gpg --no-tty -d %(ledger-file) | ledger -f - bal assets:bank liabilities:card")
        ("bal" "gpg --no-tty -d %(ledger-file) | ledger -f - bal")
        ("reg" "gpg --no-tty -d %(ledger-file) | ledger -f - reg")
        ("payee" "gpg --no-tty -d %(ledger-file) ledger -f - reg @%(payee)")
        ("account" "gpg --no-tty -d %(ledger-file) | ledger -f - reg %(account)")))

(provide 'init-ledger-mode)

;;; init-ledger-mode ends here
