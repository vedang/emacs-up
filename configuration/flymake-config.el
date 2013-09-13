;;; settings for flymake
(when (load "flymake" t)
  ;; http://git.vo20.nl/
  ;; Fix for nasty `cannot open doc string file` flymake error
  (defun flymake-create-temp-intemp (file-name prefix)
    "Return file name in temporary directory for checking FILE-NAME.
This is a replacement for `flymake-create-temp-inplace'. The
difference is that it gives a file name in
`temporary-file-directory' instead of the same directory as
FILE-NAME.

For the use of PREFIX see that function.

Note that not making the temporary file in another directory
\(like here) will not if the file you are checking depends on
relative paths to other files \(for the type of checks flymake
makes)."
    (unless (stringp file-name) (error "Invalid file-name"))
    (or prefix (setq prefix "flymake"))
    (let* ((name (concat
                  (file-name-nondirectory
                   (file-name-sans-extension file-name))
                  "_" prefix))
           (ext  (concat "." (file-name-extension file-name)))
           (temp-name (make-temp-file name nil ext)))
      (flymake-log 3 "create-temp-intemp: file=%s temp=%s" file-name temp-name)
      temp-name))

  ;; Flymake for Python
  (defun flymake-pyflakes-init ()
    (let* ((temp-file (flymake-init-create-temp-buffer-copy
                       'flymake-create-temp-intemp))
           (local-file (file-relative-name
                        temp-file
                        (file-name-directory buffer-file-name))))
      (list "pycheckers"  (list local-file))))
  (add-to-list 'flymake-allowed-file-name-masks
               '("\\.py\\'" flymake-pyflakes-init))

  ;; Flymake for Erlang
  (defun flymake-erlang-init ()
    (let* ((temp-file (flymake-init-create-temp-buffer-copy
                       'flymake-create-temp-intemp))
           (local-file (file-relative-name temp-file
                        (file-name-directory buffer-file-name))))
      (list "erlcheckers" (list local-file))))
  (add-to-list 'flymake-allowed-file-name-masks
               '("\\.erl\\'" flymake-erlang-init))

  ;; Flymake for LaTeX
  (defun flymake-get-tex-args (file-name)
    (list "chktex" (list "-g0" "-r" "-l"
                         (expand-file-name (concat dotfiles-dirname
                          "plugins/latex/chktexcheckers"))
                         "-I" "-q" "-v0" file-name)))
  (push
   '("^\\(\.+\.tex\\):\\([0-9]+\\):\\([0-9]+\\):\\(.+\\)"
     1 2 3 4) flymake-err-line-patterns)

  (load-library "flymake-cursor")
  (global-set-key [f3] 'flymake-goto-prev-error)
  (global-set-key [f2] 'flymake-goto-next-error)

  (defvar flymake-disabled-modes '("java" "xml" "html"))

  (defun flymake-delete-unnecessary-modes (modes-to-disable)
    "Remove modes from flymake-allowed-file-name-masks to avoid flymake popup."
    (let* ((flymake-elts-to-delete))
      (dolist (elt flymake-allowed-file-name-masks)
        (dolist (disabled-mode modes-to-disable)
          (when (eq (string-match-p (car elt) (concat "." disabled-mode)) 0)
            (add-to-list 'flymake-elts-to-delete elt))))
      (dolist (elt flymake-elts-to-delete)
        (setq flymake-allowed-file-name-masks
              (delete elt flymake-allowed-file-name-masks)))))

  (flymake-delete-unnecessary-modes flymake-disabled-modes))


(provide 'flymake-config)
