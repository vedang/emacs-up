;;; init-sql.el --- convenience functions for working with SQL. -*- lexical-binding: t -*-
;;; Author: Vedang Manerikar
;;; Created on: 19 Jul 2020
;;; Copyright (c) 2020 Vedang Manerikar <vedang.manerikar@gmail.com>

;; This file is not part of GNU Emacs.

;;; License:

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the Do What The Fuck You Want to
;; Public License, Version 2, which is included with this distribution.
;; See the file LICENSE.txt

;;; Commentary:

;;; To start a SQLi buffer + work against a database:
;; 1. M-x sql-<name-of-db> (`sql-postgres'). If you have the dir-locals
;;    file (see below), values will be auto filled in. This will start
;;    an interactive SQL buffer.
;; 2. Open a .sql file to write SQL in.
;; 3. In this file, M-x `sql-set-product', followed by M-x
;;    `sql-set-sqli-buffer'.

;;; To speed up connection to an SQL database, add the following to a
;;; `.dir-locals.el' file:

;; ((sql-mode . ((sql-product . postgres)
;;               (sql-user . "pgw")
;;               (sql-password . "")
;;               (sql-port . 5432)
;;               (sql-server . "192.168.33.10")
;;               (sql-database . "pgw-main"))))

;;; Code:

(defvar sql-history-folder
  (expand-file-name "temp-files/sql/" user-emacs-directory))

(defun emacswiki/sql-save-history-hook ()
  "Save the command history from SQLi buffers."
  (let ((lval 'sql-input-ring-file-name)
        (rval 'sql-product))
    (if (symbol-value rval)
        (let ((filename
               (concat sql-history-folder
                       (symbol-name (symbol-value rval))
                       "-history.sql")))
          (set (make-local-variable lval) filename))
      (error
       (format "SQL history will not be saved because %s is nil"
               (symbol-name rval))))))

(add-hook 'sql-interactive-mode-hook 'emacswiki/sql-save-history-hook)

;;; From:
;;; https://fluca1978.github.io/2022/04/13/EmacsPgFormatter.html, with
;;; minor modifications to add the function to a `before-save-hook`
;;; pg_format can be installed from: https://github.com/darold/pgFormatter
(defun pgformatter-on-region ()
  "A function to invoke pgFormatter as an external program."
  (interactive)
  (let ((b (if mark-active (region-beginning) (point-min)))
        (e (if mark-active (region-end) (point-max)))
        (pgfrm (executable-find "pg_format")))
    (if pgfrm
        (let ((p (point)))
          (shell-command-on-region b e pgfrm (current-buffer) t)
          (goto-char p))
      (user-error "Could not find pg_format installed"))))

(defun sql-format-buffer-on-save ()
  "When saving an SQL buffer, format it with pg_format."
  (add-hook 'before-save-hook #'pgformatter-on-region -10 t))

(add-hook 'sql-mode-hook #'sql-format-buffer-on-save)

(defun upcase-sql-keywords ()
  "Convert all SQL keywords to uppercase."
  (interactive)
  (save-excursion
    (dolist (keywords sql-mode-postgres-font-lock-keywords)
      (goto-char (point-min))
      (while (re-search-forward (car keywords) nil t)
        (goto-char (+ 1 (match-beginning 0)))
        (when (eql font-lock-keyword-face (face-at-point))
          (backward-char)
          (upcase-word 1)
          (forward-char))))))

;;; Taken from Stack overflow to deal with misaligned printing in the
;;; sqli buffer.
;; https://emacs.stackexchange.com/a/18403

;; Silence compiler warnings
(defvar sql-product)
(defvar sql-prompt-regexp)
(defvar sql-prompt-cont-regexp)

(defun emacswiki/sql-interactive-mode-hook ()
  "Custom interactive SQL mode behaviours. See `sql-interactive-mode-hook'."
  (when (eq sql-product 'postgres)
    ;; Allow symbol chars in database names in prompt.
    ;; Default postgres pattern was: (see `sql-product-alist').
    ;; :prompt-regexp "^[[:alnum:]_]*=[#>] "
    ;; :prompt-cont-regexp "^[[:alnum:]_]*[-(][#>] "
    (setq sql-prompt-regexp "^\\(?:\\sw\\|\\s_\\)*=[#>] ")
    (setq sql-prompt-cont-regexp "^\\(?:\\sw\\|\\s_\\)*[-(][#>] "))

  ;; Deal with inline prompts in query output.
  ;; Runs after `sql-interactive-remove-continuation-prompt'.
  (add-hook 'comint-preoutput-filter-functions
            'emacswiki/sql-comint-preoutput-filter
            :append :local))

(defun emacswiki/sql-comint-preoutput-filter (output)
  "Filter prompts out of SQL query OUTPUT.

Runs after `sql-interactive-remove-continuation-prompt' in
`comint-preoutput-filter-functions'."
  ;; If the entire output is simply the main prompt, return that.
  ;; (i.e. When simply typing RET at the sqli prompt.)
  (if (string-match (concat "\\`\\(" sql-prompt-regexp "\\)\\'") output)
      output
    ;; Otherwise filter all leading prompts from the output.
    ;; Store the buffer-local prompt patterns before changing buffers.
    (let ((main-prompt sql-prompt-regexp)
          (any-prompt comint-prompt-regexp) ;; see `sql-interactive-mode'
          (prefix-newline nil))
      (with-temp-buffer
        (insert output)
        (goto-char (point-min))
        (when (looking-at main-prompt)
          (setq prefix-newline t))
        (while (looking-at any-prompt)
          (replace-match ""))
        ;; Prepend a newline to the output, if necessary.
        (when prefix-newline
          (goto-char (point-min))
          (unless (looking-at "\n")
            (insert "\n")))
        ;; Return the filtered output.
        (buffer-substring-no-properties (point-min) (point-max))))))

(add-hook 'sql-interactive-mode-hook 'emacswiki/sql-interactive-mode-hook)

(defadvice sql-send-string (before my-prefix-newline-to-sql-string)
  "Force all `sql-send-*' commands to include an initial newline.

This is a trivial solution to single-line queries tripping up my
custom output filter.  (See `emacswiki/sql-comint-preoutput-filter'.)"
  (ad-set-arg 0 (concat "\n" (ad-get-arg 0))))

(ad-activate 'sql-send-string)

(provide 'init-sql)
;;; init-sql.el ends here
