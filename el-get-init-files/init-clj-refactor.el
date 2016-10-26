;;; init-clj-refactor.el --- Magic refactoring for Clojure.
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


(defun turn-on-clj-refactor ()
  (clj-refactor-mode 1)
  (cljr-add-keybindings-with-prefix "C-c m"))

(setq cljr-favor-prefix-notation nil ; don't mess up namespaces on cleanup
      ;; don't build asts on starup, this leads to running the test
      ;; suite on connect.
      cljr-eagerly-build-asts-on-startup nil
      ;; don't stop on analyzer failures. examples: not finding a
      ;; data-reader.
      cljr-ignore-analyzer-errors t)

(eval-after-load 'clojure-mode
  '(progn
     (add-hook 'clojure-mode-hook 'turn-on-clj-refactor)))


(provide 'init-clj-refactor)
