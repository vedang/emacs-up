;;; js-beautify.el -- beautify some js code

(defgroup js-beautify nil
  "Use jsbeautify to beautify some js"
  :group 'editing)

(defcustom js-beautify-args "--jslint-happy --brace-style=end-expand --keep-array-indentation"
  "Arguments to pass to jsbeautify script"
  :type '(string)
  :group 'js-beautify)

(defcustom js-beautify-path "~/.emacs.d/plugins/jsbeautifier.py"
  "Path to jsbeautifier python file"
  :type '(string)
  :group 'js-beautify)

(defun js-beautify ()
  "Beautify a region of javascript using the code from jsbeautify.org"
  (interactive)
  (let ((orig-point (point)))
    (unless (mark)
      (mark-defun))
    (shell-command-on-region (point)
                             (mark)
                             (concat "python "
                                     js-beautify-path
                                     " --stdin "
                                     js-beautify-args)
                             nil t)
    (goto-char orig-point)))

(provide 'js-beautify)
;;; js-beautify.el ends here
