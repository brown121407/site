;;; publish.el --- description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2020 Alexandru-Sergiu Marton
;;
;; Author: Alexandru-Sergiu Marton <http://github/brown>
;; Maintainer: Alexandru-Sergiu Marton <brown121407@posteo.ro>
;; Created: July 03, 2020
;; Modified: July 03, 2020
;; Version: 0.0.1
;; Keywords:
;; Homepage: https://github.com/brown/publish
;; Package-Requires: ((emacs 26.3) (cl-lib "0.5"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  description
;;
;;; Code:

(require 'package)
(package-initialize)

(require 'org)
(require 'ox)
(require 'ox-publish)
(require 'xml)
(require 'dom)

(setq b7-url "https://brown.121407.xyz")
(setq b7-title "brown121407")
(setq b7-root (file-name-directory (or load-file-name buffer-file-name)))

(defun b7-org-html-close-tag (tag &rest attrs)
  "Return close-tag for string TAG.
ATTRS specify additional attributes."
  (concat "<" tag " "
          (mapconcat (lambda (attr)
                       (format "%s=\"%s\"" (car attr) (cadr attr)))
                     attrs
                     " ")
          ">"))

(defun b7-html-head-extra (file project)
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
               `(,(b7-org-html-close-tag "link" '(rel icon) '(type image/x-icon) `(href ,favicon))
                 ,(b7-org-html-close-tag "meta" '(property og:title) `(content ,title))
                 ,(b7-org-html-close-tag "meta" '(property og:url) `(content ,full-url))
                 ,(and description
                       (b7-org-html-close-tag "meta" '(property og:description) `(content ,description)))
                 ,(b7-org-html-close-tag "meta" '(property og:image) `(content ,image))
                 ,(b7-org-html-close-tag "meta" '(property og:type) `(content ,type))
                 ,(and (equal type "article")
                       (b7-org-html-close-tag "meta" '(property article:author) `(content ,author)))
                 ,(and (equal type "article")
                       (b7-org-html-close-tag "meta" '(property article:published_time) `(content ,(format-time-string "%FT%T%z" date))))

                 ,(b7-org-html-close-tag "meta" '(property twitter:title) `(content ,title))
                 ,(b7-org-html-close-tag "meta" '(property twitter:url) `(content ,full-url))
                 ,(b7-org-html-close-tag "meta" '(property twitter:image) `(content ,image))
                 ,(and description
                       (b7-org-html-close-tag "meta" '(property twitter:description) `(content ,description)))
                 ,(and description
                       (b7-org-html-close-tag "meta" '(property twitter:card) '(content summary)))
                 ,(b7-org-html-close-tag "link" '(rel stylesheet) '(href "/assets/css/style.css"))
                 )
               "\n")))

(defun b7-org-html-publish-to-html (plist filename pub-dir)
  "Wrapper function to publish an file to html.

PLIST contains the properties, FILENAME the source file and
  PUB-DIR the output directory."
  (let ((project (cons 'b7 plist)))
    (plist-put plist :html-head-extra
               (b7-html-head-extra filename project))
    (org-html-publish-to-html plist filename pub-dir)))

(defun b7-org-html-format-headline-function (todo todo-type priority text tags info)
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

(defun b7-org-publish-sitemap (title list)
  "Generate sitemap as a string, having TITLE.
LIST is an internal representation for the files to include, as
returned by `org-list-to-lisp'."
  (let ((filtered-list (cl-remove-if (lambda (x)
                                       (and (sequencep x) (null (car x))))
                                     list)))
    (org-list-to-org filtered-list)))

(defun b7-org-publish-sitemap-entry (entry style project)
  "Format for sitemap ENTRY, as a string.
ENTRY is a file name.  STYLE is the style of the sitemap.
PROJECT is the current project."
  (unless (equal entry "index.org")
    (format "%s --- [[file:%s][%s]]"
            (format-time-string "%Y-%m-%d" (org-publish-find-date entry project))
            entry
            (org-publish-find-title entry project))))

(defun b7-format-rss-feed-entry (entry style project)
  "Format ENTRY for the RSS feed.
ENTRY is a file name.  STYLE is either 'list' or 'tree'.
PROJECT is the current project."
  (cond ((not (directory-name-p entry))
         (let* ((file (org-publish--expand-file-name entry project))
                (title (org-publish-find-title entry project))
                (date (format-time-string "%Y-%m-%d" (org-publish-find-date entry project)))
                (link (concat (file-name-sans-extension entry) ".html")))
           (with-temp-buffer
             (insert (format "* [[file:%s][%s]]\n" file title))
             (org-set-property "RSS_PERMALINK" link)
             (org-set-property "RSS_TITLE" title)
             (org-set-property "PUBDATE" date)
             (insert-file-contents file)
             (buffer-string))))
        ((eq style 'tree)
         ;; Return only last subdir.
         (file-name-nondirectory (directory-file-name entry)))
        (t entry)))

(defun b7-format-rss-feed (title list)
  "Generate RSS feed, as a string.
TITLE is the title of the RSS feed.  LIST is an internal
representation for the files to include, as returned by
`org-list-to-lisp'.  PROJECT is the current project."
  (concat "#+TITLE: " title "\n\n"
          (org-list-to-subtree list 1 '(:icount "" :istart ""))))

(defun b7-org-rss-publish-to-rss (plist filename pub-dir)
  "Publish RSS with PLIST, only when FILENAME is 'rss.org'.
PUB-DIR is when the output will be placed."
  (if (equal "rss.org" (file-name-nondirectory filename))
      (org-rss-publish-to-rss plist filename pub-dir)))

(defun project-dir (name)
  (expand-file-name name b7-root))

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
      (if (not lang) (format "<pre class=\"example\"%s>\n%s</pre>" label code)
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

(defun org-rss-final-function (contents backend info)
  "Prettify the RSS output."
  (with-temp-buffer
    (xml-mode)
    (insert contents)
;;    (indent-region (point-min) (point-max))
    (buffer-substring-no-properties (point-min) (point-max))))

(setq b7-project-alist
      `(("pages"
         :base-directory ,(project-dir "")
         :base-extension "org"
         :recursive nil
         :publishing-directory ,(project-dir "public")
         :publishing-function b7-org-html-publish-to-html
         :html-link-home "/"
         :html-home/up-format ""
         :html-format-headline-function b7-org-html-format-headline-function
         :author "Alexandru-Sergiu Marton"
         :email "brown121407@posteo.ro"
         :with-creator t)
        ("posts"
         :base-directory ,(project-dir "posts")
         :base-extension "org"
         :recursive nil
         :exclude ,(regexp-opt '("rss.org" "sitemap.org"))
         :publishing-directory ,(project-dir "public/posts")
         :publishing-function b7-org-html-publish-to-html
         :html-link-home "/"
         :html-home/up-format ""
         :auto-sitemap t
         :html-format-headline-function b7-org-html-format-headline-function
         :sitemap-filename "sitemap.org"
         :sitemap-title ,b7-title
         :sitemap-sort-files anti-chronologically
         :sitemap-style list
         :sitemap-function b7-org-publish-sitemap
         :sitemap-format-entry b7-org-publish-sitemap-entry
         :author "Alexandru-Sergiu Marton"
         :email "brown121407@posteo.ro"
         :with-creator t)
        ("blog-rss"
         :base-directory ,(project-dir "posts")
         :base-extension "org"
         :recursive nil
         :exclude ,(regexp-opt '("rss.org" "index.org" "sitemap.org" "404.org" "projects.org" "extra.org"))
         :publishing-function b7-org-rss-publish-to-rss
         :publishing-directory ,(project-dir "public")
         :rss-extension "xml"
         :html-link-home ,b7-url
         :html-link-use-abs-url t
         :html-link-org-files-as-html t
         :auto-sitemap t
         :sitemap-filename "rss.org"
         :sitemap-title ,b7-title
         :sitemap-style list
         :sitemap-sort-files anti-chronologically
         :sitemap-function b7-format-rss-feed
         :sitemap-format-entry b7-format-rss-feed-entry)
        ("assets"
         :base-directory ,(project-dir "assets")
         :base-extension any
         :recursive t
         :publishing-directory ,(project-dir "public/assets")
         :publishing-function org-publish-attachment
         :html-link-home "/")))

(defun b7-publish-all ()
  "."
  (interactive)
  (let ((org-publish-project-alist b7-project-alist)
        ;;(org-publish-timestamp-directory ".timestamps")
        (org-export-with-smart-quotes    t)
        (org-export-with-toc             nil)
        (org-html-doctype "html5")
        (org-html-divs '((preamble  "header" "preamble")
                         (content   "main"   "content")
                         (postamble "footer" "postamble")))
        (org-html-container-element "section")
        (org-html-link-home "/")
        (org-html-html5-fancy t)
        (org-html-checkbox-type 'html)
        (org-html-validation-link nil)
        (org-html-preamble t)
        (org-html-postamble t)
        (org-html-postamble-format '(("en" "<hr>\n<p class=\"author\">Copyright 2019, 2020 %a (%e)</p>\n<p>Source code is licensed under GNU GPLv3</p>\n<p>Content is licensed under CC-BY-SA</p>\n<p class=\"date\">Date: %d</p>\n<p class=\"creator\">%c</p>")))
        (org-html-preamble-format `(("en" ,(format "<a href=\"%s\">HOME</a> |\n<a href=\"%s\">PROJECTS</a> |\n<a href=\"%s\">EXTRA</a><hr>" "/" "/projects.html" "/extra.html")))))
    (org-publish-remove-all-timestamps)
    (org-publish-all)))

;;;
;;; publish.el ends here
