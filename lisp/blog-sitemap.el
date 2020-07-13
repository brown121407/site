;;; blog-sitemap.el --- description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2020 Alexandru-Sergiu Marton
;;
;; Author: Alexandru-Sergiu Marton <http://github/brown>
;; Maintainer: Alexandru-Sergiu Marton <brown121407@posteo.ro>
;; Created: July 13, 2020
;; Modified: July 13, 2020
;; Version: 0.0.1
;; Keywords:
;; Homepage: https://github.com/brown/blog-sitemap
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

(defun blog-org-publish-sitemap (title list)
  "Generate sitemap as a string, having TITLE.
LIST is an internal representation for the files to include, as
returned by `org-list-to-lisp'."
  (let ((filtered-list (cl-remove-if (lambda (x)
                                       (and (sequencep x) (null (car x))))
                                     list)))
    (org-list-to-org filtered-list)))

(defun blog-org-publish-sitemap-entry (entry style project)
  "Format for sitemap ENTRY, as a string.
ENTRY is a file name.  STYLE is the style of the sitemap.
PROJECT is the current project."
  (unless (equal entry "index.org")
    (format "%s --- [[file:%s][%s]]"
            (format-time-string "%Y-%m-%d" (org-publish-find-date entry project))
            entry
            (org-publish-find-title entry project))))

(provide 'blog-sitemap)
;;; blog-sitemap.el ends here
