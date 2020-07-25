;;; blog.el --- description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2020 Alexandru-Sergiu Marton
;;
;; Author: Alexandru-Sergiu Marton <http://github/brown>
;; Maintainer: Alexandru-Sergiu Marton <brown121407@posteo.ro>
;; Created: July 13, 2020
;; Modified: July 13, 2020
;; Version: 0.0.1
;; Keywords:
;; Homepage:
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
(require 'blog-html)
(require 'blog-sitemap)
(require 'blog-rss)

(defun parent-dir (file)
  "Return the parent directory of FILE."
  (unless (equal "/" file)
    (file-name-directory (directory-file-name file))))

(defvar blog-url "https://example.org")
(defvar blog-title "My Blog")
(defvar blog-root (parent-dir (or load-file-name buffer-file-name)))
(defvar blog-author-name user-full-name)
(defvar blog-author-email user-mail-address)
(defvar blog-content-license "CC-BY-SA")
(defvar blog-code-license "GNU GPLv3")

(defun project-dir (&optional dir)
  "Get the absolute path of DIR as if it is a directory in BLOG-ROOT."
  (expand-file-name (or dir "") blog-root))

(defun root-link (link)
  "Append LINK to BLOG-ROOT."
  (concat (file-name-as-directory blog-url) link))

(defun get-blog-project-alist ()
      `(("posts"
         :base-directory ,(project-dir "posts")
         :base-extension "org"
         :recursive nil
         :exclude ,(regexp-opt '("rss.org" "sitemap.org"))
         :publishing-directory ,(project-dir "public/posts")
         :publishing-function blog-org-html-publish-to-html
         :html-link-home "/"
         :html-home/up-format ""
         :auto-sitemap t
         :html-format-headline-function blog-org-html-format-headline-function
         :sitemap-filename "sitemap.org"
         :sitemap-title ,blog-title
         :sitemap-sort-files anti-chronologically
         :sitemap-style list
         :sitemap-function blog-org-publish-sitemap
         :sitemap-format-entry blog-org-publish-sitemap-entry
         :author ,blog-author-name
         :email ,blog-author-email
         :with-creator t)
        ("pages"
         :base-directory ,(project-dir "")
         :base-extension "org"
         :recursive nil
         :publishing-directory ,(project-dir "public")
         :publishing-function blog-org-html-publish-to-html
         :html-link-home "/"
         :html-home/up-format ""
         :html-format-headline-function blog-org-html-format-headline-function
         :author ,blog-author-name
         :email ,blog-author-email
         :with-creator t)
        ("blog-rss"
         :base-directory ,(project-dir "posts")
         :base-extension "org"
         :recursive nil
         :exclude ,(regexp-opt '("rss.org" "index.org" "sitemap.org" "404.org" "projects.org" "extra.org"))
         :publishing-function blog-org-rss-publish-to-rss
         :publishing-directory ,(project-dir "public")
         :rss-extension "xml"
         :html-link-home ,blog-url
         :html-link-use-abs-url t
         :html-link-org-files-as-html t
         :auto-sitemap t
         :sitemap-filename "rss.org"
         :sitemap-title ,blog-title
         :sitemap-style list
         :sitemap-sort-files anti-chronologically
         :sitemap-function blog-format-rss-feed
         :sitemap-format-entry blog-format-rss-feed-entry)
        ("assets"
         :base-directory ,(project-dir "assets")
         :base-extension any
         :recursive t
         :publishing-directory ,(project-dir "public/assets")
         :publishing-function org-publish-attachment)))

(defun blog-publish-all ()
  "Publish the blog."
  (interactive)
  (let ((org-publish-project-alist (get-blog-project-alist))
        ;;(org-publish-timestamp-directory ".timestamps")
        (org-export-with-smart-quotes    t)
        (org-export-with-toc             nil)
        (org-html-doctype "html5")
        (org-html-divs '((preamble  "header" "preamble")
                         (content   "main"   "content")
                         (postamble "footer" "postamble")))
        (org-html-container-element "section")
        (org-html-link-home blog-root)
        (org-html-html5-fancy t)
        (org-html-checkbox-type 'html)
        (org-html-validation-link nil)
        (org-html-preamble t)
        (org-html-postamble t)
        (org-html-postamble-format `(("en"
                                      ,(format "<hr>\n
<p class=\"proles\"><a href=\"https://www.marxists.org/\"><i>Workers of the world, unite!</i></a></p>\n
<p class=\"author\">Copyright 2019, 2020 %%a (%%e)</p>\n
<p>Source code is licensed under %s</p>\n
<p>Content is licensed under %s</p>\n
<p class=\"date\">Date: %%d</p>\n
<p class=\"creator\">%%c</p>"
                                              blog-code-license
                                              blog-content-license))))
        (org-html-preamble-format `(("en"
                                     ,(format "<a href=\"%s\">HOME</a> <span class=\"pre-sep\">|</span>\n
<a href=\"%s\">PROJECTS</a> <span class=\"pre-sep\">|</span>\n
<a href=\"%s\">EXTRA</a><hr>"
                                              "/"
                                              "/projects.html"
                                              "/extra.html")))))
    (org-publish-remove-all-timestamps)
    (delete-directory (project-dir "public") t)
    (org-publish-all)
    (message "Done publishing.")))

(provide 'blog)
;;;
;;; blog.el ends here
