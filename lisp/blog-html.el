;;; blog-html.el --- description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2020 Alexandru-Sergiu Marton
;;
;; Author: Alexandru-Sergiu Marton <http://github/brown>
;; Maintainer: Alexandru-Sergiu Marton <brown121407@posteo.ro>
;; Created: July 13, 2020
;; Modified: July 13, 2020
;; Version: 0.0.1
;; Keywords:
;; Homepage: https://github.com/brown/blog-html
;; Package-Requires: ((emacs 26.3) (cl-lib "0.5"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  description
;;
;;; Code:

(require 'org)
(require 'ox)
(require 'ox-html)

(defun org-html-table (table contents info)
  "Transcode a TABLE element from Org to HTML.
CONTENTS is the contents of the table.  INFO is a plist holding
contextual information."
  (if (eq (org-element-property :type table) 'table.el)
      ;; "table.el" table.  Convert it using appropriate tools.
      (org-html-table--table.el-table table info)
    ;; Standard table.
    (let* ((caption (org-export-get-caption table))
           (number (org-export-get-ordinal
                    table info nil #'org-html--has-caption-p))
           (attributes
            (org-html--make-attribute-string
             (org-combine-plists
              (and (org-element-property :name table)
                   (list :id (org-export-get-reference table info)))
              (and (not (org-html-html5-p info))
                   (plist-get info :html-table-attributes))
              (org-export-read-attribute :attr_html table))))
           (alignspec
            (if (bound-and-true-p org-html-format-table-no-css)
                "align=\"%s\""
              "class=\"org-%s\""))
           (table-column-specs
            (lambda (table info)
              (mapconcat
               (lambda (table-cell)
                 (let ((alignment (org-export-table-cell-alignment
                                   table-cell info)))
                   (concat
                    ;; Begin a colgroup?
                    (when (org-export-table-cell-starts-colgroup-p
                           table-cell info)
                      "\n<colgroup>")
                    ;; Add a column.  Also specify its alignment.
                    (format "\n%s"
                            (org-html-close-tag
                             "col" (concat " " (format alignspec alignment)) info))
                    ;; End a colgroup?
                    (when (org-export-table-cell-ends-colgroup-p
                           table-cell info)
                      "\n</colgroup>"))))
               (org-html-table-first-row-data-cells table info) "\n"))))
      (format "<div class=\"table-container\"><table%s>\n%s\n%s\n%s</table></div>"
              (if (equal attributes "") "" (concat " " attributes))
              (if (not caption) ""
                (format (if (plist-get info :html-table-caption-above)
                            "<caption class=\"t-above\">%s</caption>"
                          "<caption class=\"t-bottom\">%s</caption>")
                        (concat
                         "<span class=\"table-number\">"
                         (format (org-html--translate "Table %d:" info) number)
                         "</span> " (org-export-data caption info))))
              (funcall table-column-specs table info)
              contents))))

(defun org-html-src-block (src-block _contents info)
  "Transcode a SRC-BLOCK element from Org to HTML.
CONTENTS holds the contents of the item.  INFO is a plist holding
contextual information."
  (if (org-export-read-attribute :attr_html src-block :textarea)
      (org-html--textarea-block src-block)
    (let* ((lang (org-element-property :language src-block))
           (code (org-html-format-code src-block info))
           (label (let ((lbl (and (org-element-property :name src-block)
                                  (org-export-get-reference src-block info))))
                    (if lbl (format " id=\"%s\"" lbl) "")))
           (klipsify  (and  (plist-get info :html-klipsify-src)
                            (member lang '("javascript" "js"
                                           "ruby" "scheme" "clojure" "php" "html")))))
      (if (not lang) (format "<div class=\"org-src-container\">\n
<pre class=\"example\"%s><pre class=\"inside-src\">%s</pre></pre></div>" label code)
        (format "<div class=\"org-src-container\">\n%s%s\n</div>"
                ;; Build caption.
                (let ((caption (org-export-get-caption src-block)))
                  (if (not caption) ""
                    (let ((listing-number
                           (format
                            "<span class=\"listing-number\">%s </span>"
                            (format
                             (org-html--translate "Listing %d:" info)
                             (org-export-get-ordinal
                              src-block info nil #'org-html--has-caption-p)))))
                      (format "<label class=\"org-src-name\">%s%s</label>"
                              listing-number
                              (org-trim (org-export-data caption info))))))
                ;; Contents.
                (if klipsify
                    (format "<pre><code class=\"src src-%s\"%s%s>%s</code></pre>"
                            lang
                            label
                            (if (string= lang "html")
                                " data-editor-type=\"html\""
                              "")
                            code)
                  (format "<pre class=\"src src-%s\"%s><pre class=\"inside-src\">%s</pre></pre>"
                          lang label code)))))))

(defun blog-org-html-close-tag (tag &rest attrs)
  "Return close-tag for string TAG.
ATTRS specify additional attributes."
  (concat "<" tag " "
          (mapconcat (lambda (attr)
                       (format "%s=\"%s\"" (car attr) (cadr attr)))
                     attrs
                     " ")
          ">"))

(defun blog-html-head-extra (file project)
  "FILE PROJECT."
  (let* ((info (cdr project))
         ;; (org-export-options-alist
         ;;  `((:title "TITLE" nil nil parse)
         ;;    (:date "DATE" nil nil parse)
         ;;    (:author "AUTHOR" nil ,(plist-get info :author) space)
         ;;    (:description "DESCRIPTION" nil nil newline)
         ;;    (:keywords "KEYWORDS" nil nil space)
         ;;    (:meta-image "META_IMAGE" nil ,(plist-get info :meta-image) nil)
         ;;    (:meta-type "META_TYPE" nil ,(plist-get info :meta-type) nil)))
         (title (org-publish-find-title file project))
         (date (org-publish-find-date file project))
         (author (org-publish-find-property file :author project))
         (description (org-publish-find-property file :description project))
         (link-home (file-name-as-directory (plist-get info :html-link-home)))
         (extension (or (plist-get info :html-extension) org-html-extension))
         (rel-file (org-publish-file-relative-name file info))
         (full-url (concat link-home (file-name-sans-extension rel-file) "." extension))
         (image (concat link-home (org-publish-find-property file :meta-image project)))
         (favicon (concat link-home "favicon.ico"))
         (type (org-publish-find-property file :meta-type project)))
    (mapconcat 'identity
               `(,(blog-org-html-close-tag "link" '(rel icon) '(type image/x-icon) `(href ,favicon))
                 ,(blog-org-html-close-tag "meta" '(property og:title) `(content ,title))
                 ,(blog-org-html-close-tag "meta" '(property og:url) `(content ,full-url))
                 ,(and description
                       (blog-org-html-close-tag "meta" '(property og:description) `(content ,description)))
                 ,(blog-org-html-close-tag "meta" '(property og:image) `(content ,image))
                 ,(blog-org-html-close-tag "meta" '(property og:type) `(content ,type))
                 ,(and (equal type "article")
                       (blog-org-html-close-tag "meta" '(property article:author) `(content ,author)))
                 ,(and (equal type "article")
                       (blog-org-html-close-tag "meta" '(property article:published_time) `(content ,(format-time-string "%FT%T%z" date))))

                 ,(blog-org-html-close-tag "meta" '(property twitter:title) `(content ,title))
                 ,(blog-org-html-close-tag "meta" '(property twitter:url) `(content ,full-url))
                 ,(blog-org-html-close-tag "meta" '(property twitter:image) `(content ,image))
                 ,(and description
                       (blog-org-html-close-tag "meta" '(property twitter:description) `(content ,description)))
                 ,(and description
                       (blog-org-html-close-tag "meta" '(property twitter:card) '(content summary)))
                 ,(blog-org-html-close-tag "link" '(rel stylesheet) '(href "/assets/css/style.css"))
                 ,(blog-org-html-close-tag "link" '(rel stylesheet) '(href "https://fonts.googleapis.com/css2?family=Roboto:wght@400;500&display=swap"))
                 )
               "\n")))

(defun blog-org-html-publish-to-html (plist filename pub-dir)
  "Wrapper function to publish an file to html.

PLIST contains the properties, FILENAME the source file and
  PUB-DIR the output directory."
  (let ((project (cons 'b7 plist)))
    (plist-put plist :html-head-extra
               (blog-html-head-extra filename project))
    (org-html-publish-to-html plist filename pub-dir)))

(defun blog-org-html-format-headline-function (todo todo-type priority text tags info)
  "Format a headline with a link to itself.

This function takes six arguments:
TODO      the todo keyword (string or nil).
TODO-TYPE the type of todo (symbol: ‘todo’, ‘done’, nil)
PRIORITY  the priority of the headline (integer or nil)
TEXT      the main headline text (string).
TAGS      the tags (string or nil).
INFO      the export options (plist)."
  (let* ((headline (get-text-property 0 :parent text))
         (id (or (org-element-property :CUSTOM_ID headline)
                 (org-export-get-reference headline info)
                 (org-element-property :ID headline)))
         (link (if id
                   (format "%s <a class=\"headline-ref\" href=\"#%s\">%s</a>" text id "
<svg viewBox=\"0 0 16 16\" version=\"1.1\" width=\"16\" height=\"16\" aria-hidden=\"true\">
<path fill-rule=\"evenodd\" d=\"M7.775 3.275a.75.75 0 001.06 1.06l1.25-1.25a2 2 0 112.83 2.83l-2.5 2.5a2 2 0 01-2.83 0 .75.75 0 00-1.06 1.06 3.5 3.5 0 004.95 0l2.5-2.5a3.5 3.5 0 00-4.95-4.95l-1.25 1.25zm-4.69 9.64a2 2 0 010-2.83l2.5-2.5a2 2 0 012.83 0 .75.75 0 001.06-1.06 3.5 3.5 0 00-4.95 0l-2.5 2.5a3.5 3.5 0 004.95 4.95l1.25-1.25a.75.75 0 00-1.06-1.06l-1.25 1.25a2 2 0 01-2.83 0z\"></path>
</svg>
")
                 text)))
    (org-html-format-headline-default-function todo todo-type priority link tags info)))

(provide 'blog-html)
;;; blog-html.el ends here
