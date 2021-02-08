;;; init-flycheck-clj-kondo.el --- Configuration for clj-kondo checkers
;;; Commentary:
;;; Author: Vedang Manerikar
;;; Created on: 8th Oct 2019
;;; Copyright (c) 2019 Vedang Manerikar <vedang.manerikar@gmail.com>

;; This file is not part of GNU Emacs.

;;; License:

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the Do What The Fuck You Want to
;; Public License, Version 2, which is included with this distribution.
;; See the file LICENSE.txt

;;; Code:

;; This ensures that the clj-kondo checkers are the first ones in the
;; `flycheck-checkers` list. This is needed to make the chain work. To
;; create the chain, also add the following code:

(eval-after-load 'flycheck-joker
  '(progn
    (dolist (checker '(clj-kondo-clj clj-kondo-cljs clj-kondo-cljc clj-kondo-edn))
      (setq flycheck-checkers
            (cons checker (delq checker flycheck-checkers))))

    (dolist (checkers '((clj-kondo-clj . clojure-joker)
                        (clj-kondo-cljs . clojurescript-joker)
                        (clj-kondo-cljc . clojure-joker)
                        (clj-kondo-edn . edn-joker)))
      (flycheck-add-next-checker (car checkers) (cons 'error (cdr checkers))))))

;;; init-flycheck-clj-kondo.el ends here
