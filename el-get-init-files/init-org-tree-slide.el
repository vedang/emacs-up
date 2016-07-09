;;; init-org-tree-slide.el --- Configuration for org-tree-slide
;;; Commentary:
;;; Author: Vedang Manerikar
;;; Created on: 9 Jul 2016
;;; Copyright (c) 2016 Vedang Manerikar <vedang.manerikar@gmail.com>

;; This file is not part of GNU Emacs.

;;; License:

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the Do What The Fuck You Want to
;; Public License, Version 2, which is included with this distribution.
;; See the file LICENSE.txt

;;; Code:

(eval-after-load 'org
  '(progn (define-key org-mode-map (kbd "<f8>") 'org-tree-slide-mode)))
(eval-after-load 'org-tree-slide
  '(progn (define-key org-tree-slide-mode-map (kbd "<f7>")
            'org-tree-slide-move-previous-tree)
          (define-key org-tree-slide-mode-map (kbd "<f9>")
            'org-tree-slide-move-next-tree)))

(provide 'init-org-tree-slide)
;;; init-org-tree-slide.el ends here
