;;; init-clojure-mode.el --- Configuration for Clojure Mode
;;; Author: Vedang Manerikar
;;; Created on: 27 Oct 2013
;;; Copyright (c) 2013 Vedang Manerikar <vedang.manerikar@gmail.com>

;; This file is not part of GNU Emacs.

;;; License:

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the Do What The Fuck You Want to
;; Public License, Version 2, which is included with this distribution.
;; See the file LICENSE.txt

;;; Code:


(defun icl/pretty-fns ()
  (font-lock-add-keywords
   nil `(("(\\(fn\\)[\[[:space:]]"
          (0 (progn (compose-region (match-beginning 1) (match-end 1)
                                    "ƒ")
                    nil))))))


(defun icl/pretty-reader-macros ()
  (font-lock-add-keywords
   nil `(("\\(#\\)("
          (0 (progn (compose-region (match-beginning 1) (match-end 1)
                                    ,(make-char 'greek-iso8859-7 107))
                    nil))))))


(defun icl/pretty-sets ()
  (font-lock-add-keywords
   nil `(("\\(#\\){"
          (0 (progn (compose-region (match-beginning 1) (match-end 1)
                                    "∈")
                    nil))))))


;;; Re-implementation of clojure-test-mode functions for Midje
;;; Hat-tip : Kapil Reddy
;;; https://github.com/kapilreddy/dotemacs/blob/5d6cfc2215b8f1eb2dd0ca14d871478fee053db3/configurations/clojure-config.el

(defun icl/midje-test-for (namespace)
  (let* ((namespace (clojure-underscores-for-hyphens namespace))
         (segments (split-string namespace "\\."))
         (test-segments (append (list "test") segments)))
    (mapconcat 'identity test-segments "/")))


(defun icl/midje-jump-to-test ()
  "Jump from implementation file to test."
  (interactive)
  (find-file (format "%s/%s_test.clj"
                     (file-name-as-directory
                      (locate-dominating-file buffer-file-name "src/"))
                     (icl/midje-test-for (clojure-find-ns)))))


(defun icl/midje-implementation-for (namespace)
  (let* ((namespace (clojure-underscores-for-hyphens namespace))
         (segments (split-string (replace-regexp-in-string "_test"
                                                           ""
                                                           namespace)
                                 "\\.")))
    (mapconcat 'identity segments "/")))


(defun icl/midje-jump-to-implementation ()
  "Jump from midje test file to implementation."
  (interactive)
  (find-file (format "%s/src/%s.clj"
                     (locate-dominating-file buffer-file-name "src/")
                     (icl/midje-implementation-for (clojure-find-package)))))


(defun icl/midje-jump-between-tests-and-code ()
  (interactive)
  (if (clojure-in-tests-p)
      (icl/midje-jump-to-implementation)
    (icl/midje-jump-to-test)))


(defun icl/midje-test-maybe-enable ()
  "Stop clojure-test-mode from loading, instead use my midje functions"
  (let ((ns (clojure-find-package)))
    (when (and ns (string-match "test\\(\\.\\|$\\)" ns))
      (if (and (listp clojure-mode-hook)
               (memq 'clojure-test-maybe-enable clojure-mode-hook))
          (remove-hook 'clojure-mode-hook 'clojure-test-maybe-enable)))))


(eval-after-load 'clojure-mode
  '(progn
     (put-clojure-indent 'describe 'defun)
     (put-clojure-indent 'given 'defun)
     (put-clojure-indent 'using 'defun)
     (put-clojure-indent 'given 'defun)
     (add-hook 'clojure-mode-hook 'icl/pretty-fns)
     (add-hook 'clojure-mode-hook 'icl/pretty-sets)
     (add-hook 'clojure-mode-hook 'icl/pretty-reader-macros)
     ;; I use Midje for writing clojure tests.
     ;; Activating clojure-test-mode is irritating for me.
     ;; Didn't want to change lib mode, so removing it here.
     (add-hook 'clojure-mode-hook 'icl/midje-test-maybe-enable)
     (define-key clojure-mode-map (kbd "C-c t")
       'icl/midje-jump-between-tests-and-code)
     (eval-after-load 'smartparens
       '(progn
          (define-key clojure-mode-map (kbd ")") 'sp-up-sexp)
          (define-key clojure-mode-map (kbd "]") 'sp-up-sexp)
          (define-key clojure-mode-map (kbd "}") 'sp-up-sexp)))))


(provide 'init-clojure-mode)

;;; init-clojure-mode ends here
