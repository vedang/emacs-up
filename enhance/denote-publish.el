;;; denote-publish.el --- Publish my denote files as md files  -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Vedang Manerikar

;; Author: Vedang Manerikar <vedang.manerikar@gmail.com>
;; Keywords: hypermedia, text

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This code takes different project notes, and publishes them as
;; markdown with the correct front-matter.

;;; Code:

(require 'ox-publish)
(require 'ox-md)
(require 'ox-gfm)
(require 'denote-org-extras)

;; ## The new exporter backend
;; We extend ox-gfm to get the properties we want: aliases,
;; description, subtitle

(org-export-define-derived-backend 'denote-publish 'gfm
  :translate-alist
  '((link . denote-publish-link))
  :options-alist
  '((:with-drawers nil nil nil t)
    (:aliases "ALIASES" nil nil t)
    (:description "DESCRIPTION" nil nil t)
    (:subtitle "SUBTITLE" nil nil t)
    (:identifier "IDENTIFIER" nil nil t)))

;; ## Project-specific directories
(defvar vm-base-dir)
(defvar vm-publishing-dir)

;; ## Convert the Front-Matter from org to md format.

(setq vm-base-dir (expand-file-name "~/Tresors/Documents/diary/notes/published")
      vm-publishing-dir (expand-file-name "~/src/prototypes/vedang.me/v7/components/content/resources/content"))


(defvar denote-publish--date-time-regexp
  (concat "\\`[[:digit:]]\\{4\\}-[[:digit:]]\\{2\\}-[[:digit:]]\\{2\\}"
          "\\(?:T[[:digit:]]\\{2\\}:[[:digit:]]\\{2\\}:[[:digit:]]\\{2\\}"
          "\\(?:Z\\|[+-][[:digit:]]\\{2\\}:[[:digit:]]\\{2\\}\\)*\\)*\\'")
  "Regexp to match the time stamp strings.

Reference: https://tools.ietf.org/html/rfc3339#section-5.8

Examples:
  2017-07-31
  2017-07-31T17:05:38
  2017-07-31T17:05:38Z
  2017-07-31T17:05:38+04:00
  2017-07-31T17:05:38-04:00.")

(defun denote-publish--yaml-quote-string (val)
  "Wrap VAL with quotes as appropriate.

VAL can be a string, symbol, number or nil.

VAL is returned as-it-is under the following cases:
- It is a number.
- It is a string and is already wrapped with double quotes.
- It is a string and it's value is \"true\" or \"false\".
- It is a string representing a date.
- It is a string representing an integer or float.

If VAL is nil or an empty string, a quoted empty string \"\" is
returned."
  (cond
   ((null val) val)
   ((numberp val) val)
   ((symbolp val) (format "\"%s\"" (symbol-name val)))
   ;; If `val' is a non-empty string
   ((org-string-nw-p val)
    (if (or (and (string= (substring val 0 1) "\"") ;First char is literally a "
                 (string= (substring val -1) "\"")) ;Last char is literally a "
            (string= "true" val)
            (string= "false" val)
            ;; or if it is a date (date, publishDate, expiryDate, lastmod)
            (string-match-p denote-publish--date-time-regexp val))
        val
      ;; Escape the backslashes
      (setq val (replace-regexp-in-string "\\\\" "\\\\\\\\" val))
      ;; Escape the double-quotes
      (setq val (replace-regexp-in-string "\"" "\\\\\""  val))
      (concat "\"" val "\"")))
   ;; Return empty string if anything else
   (t "\"\"")))

(defun denote-publish--get-yaml-list-string (key list)
  "Return KEY's LIST value as a YAML list, represented as a string.

KEY is a string and LIST is a list where an element can be a
symbol, number or a non-empty string.  Examples:

  \(\"abc\" \"def\")   -> \"[\\\"abc\\\", \\\"def\\\"]\"."
  (concat "["
          (mapconcat #'identity
                     (mapcar (lambda (v)
                               (denote-publish--yaml-quote-string
                                (cond
                                 ((symbolp v) (symbol-name v))
                                 ((numberp v) (number-to-string v))
                                 ((org-string-nw-p v) v)
                                 (t (user-error "Invalid element %S in `%s' value %S" v key list)))))
                             list)
                     ", ")
          "]"))

(defun denote-publish--gen-yaml-front-matter (data)
  "Generate front-matter in YAML format, and return that string.

DATA is an alist of the form \((KEY1 . VAL1) (KEY2 . VAL2) .. \),
where KEY is a symbol and VAL is a string."
  (let ((sep "---\n")
        (sign ":")
        (front-matter ""))
    (dolist (pair data)
      (let ((key (symbol-name (car pair)))
            (value (cdr pair)))
        ;; Skip writing front-matter variables whose value is nil
        (unless (or (null value) (and (stringp value) (string= "" value)))
          ;; In YAML, the value portion needs to be wrapped in double
          ;; quotes. Example:
          ;;     title: "My Post"
          (setq front-matter
                (concat front-matter
                        (format "%s%s %s\n"
                                key
                                sign
                                ;; Tags, categories, aliases:
                                ;; front-matter which are lists.
                                (if (listp value)
                                    (denote-publish--get-yaml-list-string key value)
                                  (denote-publish--yaml-quote-string value))))))))
    (concat sep front-matter sep)))

(defun denote-publish--get-front-matter (info)
  "Return the front-matter string.

INFO is a plist used as a communication channel."
  (let* ((title (org-string-nw-p (car (plist-get info :title))))
         (subtitle (org-string-nw-p (plist-get info :subtitle)))
         (description (org-string-nw-p (plist-get info :description)))
         (identifier (org-string-nw-p (plist-get info :identifier)))
         (date (org-string-nw-p (org-export-get-date info "%Y-%m-%d")))
         (last-updated-at (format-time-string "%Y-%m-%d" (current-time)))
         (aliases (when (plist-get info :aliases)
                    (org-split-string (org-string-nw-p
                                       (plist-get info :aliases))
                                      " ")))
         ;; See: [ref: do_not_use_`org-export-get-category']
         (category (org-element-map (plist-get info :parse-tree) 'keyword
	                 (lambda (kwd)
	                   (when (equal (org-element-property :key kwd) "CATEGORY")
	                     (org-element-property :value kwd)))
	                 info 'first-match))
         (data `((title . ,title)
                 (subtitle . ,subtitle)
                 (description . ,description)
                 (identifier . ,identifier)
                 (date . ,date)
                 (last_updated_at . ,last-updated-at)
                 (aliases . ,aliases)
                 (tags . ,org-file-tags)
                 (category . ,category))))
    (denote-publish--gen-yaml-front-matter data)))

;; [tag: debugging_variables]
(defvar denote-publish--tempinfo nil "Debug variable.")
(defvar denote-publish--templink nil "Debug variable.")

(defun denote-publish--link-ol-export (link description)
  "Export a `denote:' link from Org files.
The LINK, DESCRIPTION are handled by the export
backend."
  ;; (setq denote-publish--templink link)
  ;; [ref: debugging_variables]
  (let* ((path-id (denote-link--ol-resolve-link-to-target
                   (org-element-property :path link)
                   :full-data))
         (id (nth 1 path-id))
         (query (nth 2 path-id))
         (path (concat "denote:" id))
         (desc (cond
                (description)
                (query (format "%s::%s" id query))
                (t id))))
    (if query
        (format "<a href=\"%s.html%s\" class=\"internal-link\" target=\"_blank\">%s</a>"
                path query desc)
      (format "<a href=\"%s.html\" class=\"internal-link\" target=\"_blank\">%s</a>" path desc))))

(defun denote-publish-link (link desc info)
  "Convert LINK to Markdown format.

This function defers to `org-md-link' for everything other than
denote: links, which it converts to file: links.

DESC is the link's description.
INFO is a plist used as a communication channel."
  (let* ((type (org-element-property :type link)))
    (cond
     ((member type '("denote")) (denote-publish--link-ol-export link desc))
     (t (org-md-link link desc info)))))

;; ## Publish denote file to external directory

(defun denote-publish-get-front-matter (filename)
  "Get the front-matter for FILENAME"
  (let* ((org-inhibit-startup t)
	     (visiting (find-buffer-visiting filename))
	     (work-buffer (or visiting (find-file-noselect filename))))
    (unwind-protect
	    (with-current-buffer work-buffer
	      (let* ((ast (org-element-parse-buffer))
                 (info (org-combine-plists
                        (list :parse-tree ast)
                        (org-export--get-export-attributes 'denote-publish)
                        (org-export-get-environment 'denote-publish)))
                 ;; (_tempinfo (setq denote-publish--tempinfo info))
                 ;; [ref: debugging_variables]
                 )
            (denote-publish--get-front-matter info)))
      ;; Remove opened buffer in the process.
      (unless visiting (kill-buffer work-buffer)))))

(defun denote-publish-to-md (plist filename pub-dir)
  "Just like `org-md-publish-to-md' but with front-matter.

FILENAME is the filename of the Org file to be published.  PLIST
is the property list for the given project.  PUB-DIR is the
publishing directory.

Return output file name."
  (interactive)
  (let ((fm (denote-publish-get-front-matter filename))
        (outfile (org-publish-org-to 'denote-publish filename ".md" plist pub-dir)))
    (with-temp-buffer
      (insert fm)
      (insert "\n\n")
      (insert-file-contents outfile)
      (write-file outfile))
    outfile))

;; ## Configuration for `org-publish-project-alist'

(setq org-publish-project-alist
      `(("vedangme" .
         (:base-directory ,vm-base-dir
                          :publishing-directory ,vm-publishing-dir
                          :publishing-function denote-publish-to-md
                          :recursive nil
                          :exclude-tags ("noexport" "draft" "private")
                          :section-numbers nil
                          :with-creator nil
                          :with-toc nil
                          :auto-sitemap t
                          :makeindex t))))

;; # Notes
;; ## Do not use `org-export-get-category'
;; _[tag: do_not_use_`org-export-get-category']_
;;
;; We do not want the fallback behaviour of `org-export-get-category',
;; which is to return the file-name of the file as the category. For
;; us, this field only makes sense when it has been explicitly
;; defined.

(provide 'denote-publish)
;;; denote-publish.el ends here
