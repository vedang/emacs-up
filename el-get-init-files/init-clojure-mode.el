;;; init-clojure-mode.el --- Configuration for Clojure Mode -*- lexical-binding: t -*-
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

;;; Re-implementation of clojure-test-mode functions for Midje
;;; Hat-tip : Kapil Reddy
;;; https://github.com/kapilreddy/dotemacs/blob/5d6cfc2215b8f1eb2dd0ca14d871478fee053db3/configurations/clojure-config.el

(defun icl/clojure-underscores-for-hyphens (namespace)
  "Replace all hyphens in NAMESPACE with underscores."
  (replace-regexp-in-string "-" "_" namespace))


(defun icl/midje-test-for (namespace)
  (let* ((namespace (icl/clojure-underscores-for-hyphens namespace))
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
  (let* ((namespace (icl/clojure-underscores-for-hyphens namespace))
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
                     (icl/midje-implementation-for (clojure-find-ns)))))


(defun icl/clojure-in-tests-p ()
  "Check whether the current file is a test file.
  Two checks are made - whether the namespace of the file has the
  word test in it and whether the file lives under the test/
  directory."
  (or (string-match-p "test\." (clojure-find-ns))
      (string-match-p "/test" (buffer-file-name))))


(defun icl/midje-jump-between-tests-and-code ()
  (interactive)
  (if (icl/clojure-in-tests-p)
      (icl/midje-jump-to-implementation)
    (icl/midje-jump-to-test)))


;; *** DEPRECATED ***
(defun icl/midje-test-maybe-enable ()
  "Stop clojure-test-mode from loading, instead use my midje functions

  Deprecation Notice: `clojure-test-mode' no longer exists,
  making this function unnecessary. It will be removed in a
  future version."
  (let ((ns (clojure-find-ns)))
    (when (and ns (string-match "test\\(\\.\\|$\\)" ns))
      (when (and (listp clojure-mode-hook)
                 (memq 'clojure-test-maybe-enable clojure-mode-hook))
        (remove-hook 'clojure-mode-hook 'clojure-test-maybe-enable)))))


(with-eval-after-load 'clojure-mode
  (require 'clojure-mode-extra-font-locking)
  (put-clojure-indent 'describe 'defun)
  (put-clojure-indent 'given 'defun)
  (put-clojure-indent 'using 'defun)
  ;; *** DEPRECATED ***
  ;; Adding the `icl/midje-test-maybe-enable' hook is unnecessary,
  ;; since `clojure-test-mode' no longer exists. The call and
  ;; associated function will be deleted in a future commit
  (add-hook 'clojure-mode-hook 'icl/midje-test-maybe-enable)
  (add-hook 'clojure-mode-hook 'subword-mode)
  (define-key clojure-mode-map (kbd "C-c t")
              'icl/midje-jump-between-tests-and-code)
  (require 'flycheck-clj-kondo))

;;; From:
;;; https://gist.github.com/jackrusher/e628abb653429c22bc6330752b3e49a5,
;;; with minor modifications from myself.
(defun json->edn ()
  "Convert the selected region, or entire file, from JSON to EDN."
  (interactive)
  (let ((b (if mark-active (region-beginning) (point-min)))
        (e (if mark-active (region-end) (point-max)))
        (jet (when (executable-find "jet")
               "jet --pretty --keywordize keyword --from json --to edn")))
    (if jet
        (let ((p (point)))
          (shell-command-on-region b e jet (current-buffer) t)
          (goto-char p))
      (user-error "Could not find jet installed"))))

(provide 'init-clojure-mode)

;;; init-clojure-mode ends here
