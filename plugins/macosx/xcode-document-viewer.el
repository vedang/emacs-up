;;; xcode-document-viewer.el -- xcode document viewer

;; Copyright (c) 2009 IMAKADO.

;; Author: IMAKADO <ken.imakado -at-  gmail.com>
;; blog: http://d.hatena.ne.jp/IMAKADO (japanese)
;; Prefix: xcdoc:

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;; requires:
;; w3m

;; Installation:
;; (require 'xcode-document-viewer)
;; (setq xcdoc:document-path "/Developer/Platforms/iPhoneOS.platform/Developer/Documentation/DocSets/com.apple.adc.documentation.AppleiPhone3_1.iPhoneLibrary.docset")
;; (setq xcdoc:open-w3m-other-buffer t) ;if you like

;; to change docset-path
;; M-x xcdoc:set-document-path

;; to search document
;; M-x xcdoc:search

;; to search document symbol at point
;; M-x xcdoc:search-at-point

;; to select query then search
;; M-x xcdoc:ask-search


(require 'w3m-load)
(require 'thingatpt)
(require 'anything)

(defcustom xcdoc:document-path nil
  "please set docset full path like:
\"/Developer/Platforms/iPhoneOS.platform/Developer/Documentation/DocSets/com.apple.adc.documentation.AppleiPhone3_1.iPhoneLibrary.docset\"")

(defcustom xcdoc:open-w3m-other-buffer nil
  "")

(defun xcdoc:set-document-path (&optional docset-path)
  (interactive "fdocset: ")
  (cond
   ((file-readable-p docset-path)
    (or (string-match (rx ".docset") docset-path) (error "%s is not docset" docset-path))
    (setq xcdoc:document-path docset-path)
    (message "%s" docset-path))
   (t
    (error "cant read %s" docset-path))))

(defun xcdoc:document-path ()
  (cond
   (xcdoc:document-path)
   (t
    (call-interactively 'xcdoc:set-document-path))))

(defun xcdoc:docsetutil-command ()
  (or (executable-find "docsetutil")
      (and (file-executable-p "/Applications/Xcode.app/Contents/Developer/usr/bin/docsetutil") "/Applications/Xcode.app/Contents/Developer/usr/bin/docsetutil")
      (error "docsetutil command is not found. Perhaps you dont have Xcode man.")))

(defun* xcdoc:search-command (query docset)
  (format "%s search -query %s %s"
          (xcdoc:docsetutil-command)
          (shell-quote-argument query)
          docset))

(defun* xcdoc:excecute-search (&key query docset (call-shell-command-fn 'shell-command-to-string))
  "call shell command like:
\"/Developer/usr/bin/docsetutil search -query  'View'  /Developer/Platforms/iPhoneOS.platform/Developer/Documentation/DocSets/com.apple.adc.documentation.AppleiPhone3_1.iPhoneLibrary.docset\""
  (funcall call-shell-command-fn
           (xcdoc:search-command query docset)))

(defun* xcdoc:excecute-search-async (&key query docset)
  (start-process-shell-command (xcdoc:docsetutil-command)
                               (xcdoc:docsetutil-command)
                               (xcdoc:search-command query docset)))

(defsubst xcdoc:trim (s)
  (replace-regexp-in-string
   "[ \t\n]*$" "" (replace-regexp-in-string "^[ \t\n]*" "" s)))

(defun xcdoc:catdir (s1 s2)
  (let ((s1 (replace-regexp-in-string (rx "/" eol) "" s1))
        (s2 (replace-regexp-in-string (rx bol "/") "" s2)))
    (concat s1 "/" s2)))

(defun xcdoc:build-candidates-from-command-res (res)
  (let ((los (split-string res "\n")))
    (reverse
     (remove-if-not (lambda (s) (string-match (rx (group (+ (any alnum "/" "_")) ".html" (* (any alnum "/" "#" "_")))) s)) los))))

(defun xcdoc:remove-hash (s)
  (replace-regexp-in-string (rx "#//" (* not-newline)) "" s))

;; (xcdoc:extract-html "1.000 documentation/UIKit/Reference/UIView_Class/index.html")
;; (xcdoc:extract-html "Objective-C/cl/-/UIView   documentation/UIKit/Reference/UIView_Class/UIView/UIView.html#//apple_ref/occ/cl/UIView")
(defun xcdoc:extract-html (line)
  (let ((get-html-path (lambda (docpath html-return-search)
                         (xcdoc:catdir (xcdoc:catdir docpath "Contents/Resources/Documents/") html-return-search))))
    (cond
     ((string-match (rx (group (+ (any alnum "/" "_")) ".html" (* (any alnum "/" "#" "_"))))
                    line)
      (xcdoc:remove-hash
       (funcall get-html-path (xcdoc:document-path) (match-string 1 line))))
     (t
      (error "cant find text like URL!!")))))

;;(w3m-browse-url (xcdoc:extract-html" Objective-C/cl/-/UIView   documentation/UIKit/Reference/UIView_Class/UIView/UIView.html#//apple_ref/occ/cl/UIView"))
(defun xcdoc:open-w3m (url &optional new-session)
  (cond
   (xcdoc:open-w3m-other-buffer
    (let ((b (save-window-excursion (w3m-browse-url (xcdoc:extract-html url) new-session) (get-buffer "*w3m*"))))
      (ignore-errors (save-selected-window (pop-to-buffer "*w3m*")))))
   (t
    (wq3m-browse-url (xcdoc:extract-html url) new-session))))


(defun xcdoc:search-source ()
  `((name . ,(xcdoc:document-path))
    (candidates . (lambda ()
                    (xcdoc:build-candidates-from-command-res
                     (xcdoc:excecute-search
                      :query anything-pattern
                      :docset (xcdoc:document-path)))))
    (volatile)
    (delayed)
    (requires-pattern . 2)
    (action . (("w3m" . xcdoc:open-w3m)
               ("w3m new-session" . (lambda (c) (xcdoc:open-w3m c t)))))))

(defun* xcdoc:search-at-point-source-candidates
    (&optional (query (with-current-buffer anything-current-buffer
                        (or (thing-at-point 'symbol) ""))))
  (xcdoc:build-candidates-from-command-res
   (xcdoc:excecute-search
    :query query
    :docset (xcdoc:document-path))))

(defun xcdoc:search-at-point-source (&optional query)
  `((name . ,(xcdoc:document-path))
    (candidates . (lambda () (xcdoc:search-at-point-source-candidates ,query)))
    (action . (("w3m" . xcdoc:open-w3m)
               ("w3m new-session" . (lambda (c) (xcdoc:open-w3m c t)))))))

(defun xcdoc:search ()
  (interactive)
  (anything (list (xcdoc:search-source))))

(defun xcdoc:ask-search ()
  (interactive)
  (lexical-let* ((query (read-string "Query: " (or (thing-at-point 'symbol) ""))))
    (let ((anything-quit-if-no-candidate (lambda () (message "no document for %s" query))))
    (anything (list (xcdoc:search-at-point-source query))))))

(defun xcdoc:search-at-point ()
  (interactive)
  (let ((anything-quit-if-no-candidate (lambda () (message "no document for %s" (or (thing-at-point 'symbol) "")))))
    (anything (list (xcdoc:search-at-point-source)))))

(provide 'xcode-document-viewer)
;; xcode-document-viewer.el ends here.
