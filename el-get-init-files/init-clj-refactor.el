;;; init-clj-refactor.el --- Magic refactoring for Clojure.
;;; Commentary:
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
  "Helper function to add to `clojure-mode-hook'."
  (clj-refactor-mode 1)
  (cljr-add-keybindings-with-prefix "C-c m"))

(setq cljr-favor-prefix-notation nil ; don't mess up namespaces on cleanup
      ;; build asts on starup
      cljr-eagerly-build-asts-on-startup t
      cljr-warn-on-eval nil
      ;; don't stop on analyzer failures. examples: not finding a
      ;; data-reader.
      cljr-find-usages-ignore-analyzer-errors t
      ;; Don't magically add stuff to the namespace requires form
      ;; (because for big projects this operation is slow) it's easier
      ;; to do this by hand (=add-missing= operation) after you've
      ;; typed out what you wanted to.
      cljr-magic-requires nil)

(eval-after-load 'clojure-mode
  '(progn
     (add-hook 'clojure-mode-hook 'turn-on-clj-refactor)))


(provide 'init-clj-refactor)
;;; init-clj-refactor.el ends here
