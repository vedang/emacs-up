;;; init-cider.el --- Configuration for Cider.
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


(setq cider-repl-popup-stacktraces t
      cider-history-file (concat tempfiles-dirname "nrepl-history.txt")
      cider-history-size most-positive-fixnum
      nrepl-buffer-name-separator "-"
      nrepl-buffer-name-show-port t)


(eval-after-load 'cider
  '(progn
     (add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)
     (add-hook 'cider-repl-mode-hook 'subword-mode)))


(provide 'init-cider)

;;; init-cider ends here
