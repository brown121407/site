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

(add-to-list 'load-path
             (concat (file-name-directory (or load-file-name buffer-file-name)) "lisp"))

(require 'blog)

(setq blog-url "https://brown.121407.xyz"
      blog-title "brown121407"
      blog-root (parent-dir (or load-file-name buffer-file-name))
      org-rss-posts-url (concat (file-name-as-directory blog-url) "posts"))

(provide 'publish)
;;; publish.el ends here
