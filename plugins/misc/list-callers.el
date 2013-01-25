;;; list-callers.el --- Find the callers of a Lisp function
;;
;; Copyright (C) 2004, 2005  Helmut Eller
;;
;; You can redistribute this file under the terms of the GNU General
;; Public License.
;;

;;; Commentary:
;;
;; This is a little tool to find the callers of a Lisp function.  To
;; see how it work position point over an interesting function name
;; and type `M-x list-callers'.  This pops you in a window with a list
;; of the callers of that function.  (Pressing RET in that buffers
;; shows the source of the function.)
;;
;; There's also a command to display the number of callers and callees
;; of all functions in a package.  I use that to find unused
;; functions.  To see how that works type `M-x lc-show-package-summary
;; RET lc- RET'.  Main
;;
;; The tool grovels through all named function objects to see whether
;; the function references the symbol.  It's only a heuristic, but
;; works good enough for simple cases.  Things may get slow if your
;; Emacs image is very large and contains huge interlinked objects.
;;
;; The code should work with GNU Emacs 20 and Emacs 21.  XEmacs is not
;; supported.
;;

;;; Thanks:
;;
;; Andrew M. Scott for valuable feedback and pointing out that
;; function-at-point is not pre-loaded.
;;

;;; Code:

(defsubst lc-byte-code-constants (bytecode)
  "Access the constant vector of the bytecode-function BYTECODE."
  (aref bytecode 2))

(defsubst lc-node-seen-p (node seen-nodes)
  (memq node seen-nodes))

(defsubst lc-set-node-seen-p (node seen-nodes)
  (cons node seen-nodes))

(defun lc-collect-callees (symbol)
  "Return all fbound symbols referenced by the function named SYMBOL."
  (let ((seen-nodes '())
        (callees '()))
    (lc-collect-callees-guts (symbol-function symbol))
    callees))

(defvar seen-nodes) ; used to detect cycles
(defvar callees)    ; the result list

;; Here we do the actual work. seen-nodes and callees are dynamically
;; scoped so that we don't need to pass them around.  The code looks a
;; bit strange because it is tuned for efficiency.  Fbound symbols are
;; pushed to callees; for conses, vectors, and byte-functions we
;; recurse; all other objects are ignored.
(defun lc-collect-callees-guts (object)
  (cond ((symbolp object)
         (if (fboundp object)
             (push object callees)))
        ((or (numberp object) (stringp object)))
        ((if (lc-node-seen-p object seen-nodes) ; are we in a cycle?
             t
           (setq seen-nodes (lc-set-node-seen-p object seen-nodes))
           nil)
         nil)
        ((consp object)
         ;; iterate over lists ot save stack space
         (while (consp object)
           (lc-collect-callees-guts (car object))
           (setq object (cdr object))
           (cond ((lc-node-seen-p object seen-nodes)
                  (setq object nil))
                 (t
                  (setq seen-nodes (lc-set-node-seen-p object seen-nodes))))))
        ((vectorp object)
         (let ((len (length object))  (i 0))
           (while (< i len)
             (lc-collect-callees-guts (aref object i))
             (setq i (1+ i)))))
        ((byte-code-function-p object)
         (lc-collect-callees-guts (lc-byte-code-constants object)))
        ((or (bool-vector-p object) (char-table-p object)
             (bufferp object) (framep object) (subrp object)
             (overlayp object) (markerp object) (windowp object)
             (processp object))
         nil)
        (t (error "Unexpected type: %S" object))))

(defun lc-build-callees-table ()
  "Return an alist ((SYMBOL . CALLEES) ...) for all fbound symbols."
  (let ((table '()))
    (mapatoms (lambda (sym)
                (when (fboundp sym)
                  (push (cons sym (lc-collect-callees sym))
                        table))))
    table))

(defun lc-callees-rlookup (table symbol)
  "Perform a reverse lookup for SYMBOL in TABLE.
The result is a list of symbols and the symbols are callers SYMBOL.
TABLE should be a table returned by `lc-build-callees-table'."
  (let ((callers '()))
    (while table
      (if (memq symbol (cdar table))
          (push (caar table) callers))
      (setq table (cdr table)))
    callers))

(defun lc-find-callers (fsymbol)
  "Return a list of symbols for callers of the function named FSYMOBLS."
  (lc-callees-rlookup (lc-build-callees-table) fsymbol))

(defun lc-find-callees (symbol)
  "Return a list of symbols for callees of the function named FSYMOBLS."
  (lc-collect-callees symbol))

(defun lc-symbol-prefix-p (prefix symbol)
  "Is PREFIX a prefix of the symbol SYMBOL?"
  (eq (string-match prefix (symbol-name symbol)) 0))

(defun lc-function-symbols-in-package (prefix)
  "Return all fbound symbols which have the string PREFIX as a prefix."
  (let ((accu '()))
    (mapatoms (lambda (symbol)
                (if (and (fboundp symbol)
                         (lc-symbol-prefix-p prefix symbol))
                    (push symbol accu))))
    accu))

(defun lc-package-summary (package)
  "Return a list of the form ((SYMBOL CALLERS CALLEES) ...).
Each SYMBOL has the string PACKAGE as prefix."
  (let ((symbols (lc-function-symbols-in-package package))
        (table (lc-build-callees-table)))
    (mapcar (lambda (sym)
              (list sym
                    (lc-callees-rlookup table sym)
                    (cdr (assq sym table))))
            symbols)))


;;; User interface code

(defvar lc-old-window-config nil
  "Buffer local variable use to restore the window configuration.")

(defun lc-find-function-at-point-other-window (name)
  "Display the source of the function at point in other window."
  (interactive (list (thing-at-point 'symbol)))
  (unless name
    (error "No symbol at point"))
  (let* ((symbol (intern-soft name))
         (find (cond ((fboundp symbol) 'find-function-noselect)
                     ((boundp symbol) 'find-variable-noselect)
                     (t (error "Symbol not bound: %S" symbol))))
         (r (save-excursion (funcall find symbol)))
         (buffer (car r))
         (point (cdr r)))
    (cond (point
           (elisp-push-point-marker)
           (switch-to-buffer buffer)
           (goto-char point)
           (recenter 1))
          (t
           (error "Found no definition for %s in %s" name buffer)))))

(defvar lc-browser-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") 'lc-find-function-at-point-other-window)
    (define-key map "q" 'lc-quit)
    map))

(defun lc-display-callers (callers)
  "Display a buffer to browse a list of CALLERS."
  (with-current-buffer (get-buffer-create "*callers*")
    (setq buffer-read-only nil)
    (erase-buffer)
    (set (make-local-variable 'lc-old-window-config)
         (current-window-configuration))
    (use-local-map lc-browser-keymap)
    (dolist (symbol callers)
      (let ((start (point)))
        (insert (symbol-name symbol) "\n")))
    (goto-char (point-min))
    (setq buffer-read-only t)
    (select-window (display-buffer (current-buffer)))))

(defun lc-quit ()
  "Kill the *callers* buffer and restore the window configuration."
  (interactive)
  (let ((buffer (current-buffer)))
    (set-window-configuration lc-old-window-config)
    (kill-buffer buffer)))

(defun lc-function-kind (symbol)
  "Return a symbol describing the kind of function with name SYMBOL."
  (let ((fun (symbol-function symbol)))
    (cond ((commandp symbol) 'command)
          ((subrp fun) 'builtin)
          ((symbolp fun) 'alias)
          ((memq 'byte-compile-inline-expand (symbol-plist symbol))
           'inline)
          ((byte-code-function-p fun) 'compiled)
          ((consp fun) (car fun))
          (t 'function))))

(defun lc-sort-summary (summary)
  "Sort SUMMARY by the numbers of callers.
SUMMARY should be a list returned by `lc-package-summary'."
  (sort (copy-sequence summary)
        (lambda (x y)
          (let ((xlen (length (cadr x)))
                (ylen (length (cadr y))))
            (cond ((> xlen ylen) t)
                  ((< xlen ylen) nil)
                  (t (string< (car x) (car y))))))))

(defun lc-show-package-summary (package)
  "Display caller and callee counts for the functions in PACKAGE."
  (interactive "sPrefix for package: ")
  (message "Building summary...")
  (let ((summary (lc-sort-summary (lc-package-summary package))))
    (message nil)
    (with-current-buffer (get-buffer-create "*package summary*")
      (erase-buffer)
      (emacs-lisp-mode)
      (setq truncate-lines t)
      (insert "Callers  Callees  Kind      Symbol\n")
      (insert "-------  -------  ----      ------\n")
      (dolist (line summary)
        (let ((sym (car line))
              (callers (cadr line))
              (callees (nth 2 line)))
          (insert (format "%7d  %7d  %-8s  %s\n"
                          (length callers) (length callees)
                          (lc-function-kind sym)
                          sym))))
      (goto-line 3)
      (switch-to-buffer (current-buffer)))))

(defun lc-read-function-name ()
  "Read a function name much like C-h f does.  Return a symbol."
  (let* ((default (function-called-at-point))
         (string (completing-read
                  (cond (default
                          (format "List callers (default %s): " default))
                        (t "List callers: "))
                  obarray nil nil nil nil (symbol-name default))))
    (when (equal string "")
      (error "No function name specified"))
    (intern string)))

;;;###autoload
(defun list-callers (symbol)
  "List the callers of the function at point.
If called non-interactively display the callers of SYMBOL."
  (interactive (list (lc-read-function-name)))
  (cond ((or (not symbol)
             (not (symbolp symbol)))
         (error "Bad argument: %S" symbol))
        (t
         (let* ((callers (lc-find-callers symbol))
                (callers (sort callers #'string<)))
           (lc-display-callers callers)))))

(let ((byte-compile-warnings '()))
  (mapc #'byte-compile
        '(lc-collect-callees-guts
          lc-callees-rlookup
          lc-build-callees-table)))

(provide 'list-callers)

;;; list-callers.el ends here
