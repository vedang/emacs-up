;;; log4j-mode-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (log4j-mode) "log4j-mode" "log4j-mode.el" (19396
;;;;;;  13019))
;;; Generated autoloads from log4j-mode.el
 (add-to-list 'auto-mode-alist '("\\.log\\'" . log4j-mode))

(autoload 'log4j-mode "log4j-mode" "\
Major mode for viewing log files.
Log4j mode provides syntax highlighting and filtering of log files.
It also provides functionality to find and display the declaration
of a Java identifier found in the log file.

You can customize the faces that are used for syntax highlighting.
Type `M-x customize-group' and enter group name \"log4j-mode\".
You can also customize the regular expressions that are used to find
the beginning and end of multi-line log records. However, in many
cases this will not be necessary.

Commands:
Use `\\<log4j-mode-map>\\[log4j-start-filter]' to start/stop log file filtering in the current buffer.
Enter any number of include and exclude keywords that will be used to
filter the log records. Keywords are separated by spaces.

Use `\\<log4j-mode-map>\\[log4j-browse-source]' to show the declaration of the Java identifier around or
before point. This command is only enabled if package `jtags' is loaded.
For more information about jtags, see http://jtags.sourceforge.net.

Finally, the commands `\\<log4j-mode-map>\\[log4j-forward-record]' and `\\<log4j-mode-map>\\[log4j-backward-record]' move point forward and backward
across log records.

\\{log4j-mode-local-map}

\(fn)" t nil)

;;;***

;;;### (autoloads nil nil ("log4j-mode-pkg.el") (19396 13019 969000))

;;;***

(provide 'log4j-mode-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; log4j-mode-autoloads.el ends here
