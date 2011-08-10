;;; settings for flymake
(when (load "flymake" t)
  ;; Flymake for Python
  (defun flymake-pyflakes-init ()
    (let* ((temp-file (flymake-init-create-temp-buffer-copy
                       'flymake-create-temp-inplace))
           (local-file (file-relative-name
                        temp-file
                        (file-name-directory buffer-file-name))))
      (list "pycheckers"  (list local-file))))

  ;; Flymake for Erlang
  (defun flymake-erlang-init ()
    (let* ((temp-file (flymake-init-create-temp-buffer-copy
                       'flymake-create-temp-inplace))
           (local-file (file-relative-name temp-file
                        (file-name-directory buffer-file-name))))
      (list "erlcheckers" (list local-file))))

  ;; Flymake for LaTeX
  (defun flymake-get-tex-args (file-name)
    (list "chktex" (list "-g0" "-r" "-l"
                         (expand-file-name (concat dotfiles-dir
                          "plugins/latex/chktexcheckers"))
                         "-I" "-q" "-v0" file-name)))


  (add-to-list 'flymake-allowed-file-name-masks
               '("\\.py\\'" flymake-pyflakes-init))
  (add-to-list 'flymake-allowed-file-name-masks
               '("\\.erl\\'" flymake-erlang-init))
  (push
   '("^\\(\.+\.tex\\):\\([0-9]+\\):\\([0-9]+\\):\\(.+\\)"
     1 2 3 4) flymake-err-line-patterns)

  (load-library "flymake-cursor")
  (global-set-key [f3] 'flymake-goto-prev-error)
  (global-set-key [f2] 'flymake-goto-next-error)
  (delete '("\\.html?\\'" flymake-xml-init) flymake-allowed-file-name-masks))


(provide 'flymake-config)
