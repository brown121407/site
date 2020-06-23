(require 'package)
(package-initialize)

(require 'ox)
(require 'f)

(setq org-publish-project-alist
      '(("pages"
         :base-directory "pages"
         :base-extension "org"
         :publishing-directory "public"
         :publishing-function org-html-publish-to-html)
        ("posts"
         :base-directory "posts"
         :base-extension "org"
         :publishing-directory "public/posts"
         :publishing-function org-html-publish-to-html)
        ("assets"
         :base-directory "assets"
         :base-extension any
         :recursive t
         :publishing-directory "public/assets"
         :publishing-function org-publish-attachment)
        ("extra"
         :base-directory "extra"
         :base-extension any
         :recursive t
         :publishing-directory "public/extra"
         :publishing-function org-publish-attachment)
        ("website" :components ("pages" "posts" "assets" "extra"))))

(setq geiser-default-implementation 'guile
      org-export-with-toc nil
      org-html-head
      "<link rel='stylesheet' type='text/css' href='/assets/css/style.css'>"
      org-html-preamble "
<header>
    <nav>
        <a href='/'>HOME</a> |
        <a href='/projects.html'>PROJECTS</a> |
        <a href='/donate.html'>DONATE</a> |
        <a href='/extra.html'>EXTRA</a>
    </nav>
    <hr>
</header>
")

(setq webring (f-read "includes/webring-out.html"))

(setq marquee-shit "<marquee>
  Copyright 2019, 2020 Alexandru-Sergiu Marton |
  <a href='https://git.sr.ht/~brown121407/brown.121407.xyz' target=\"_blank\">Source code</a> is licensed under <a rel=\"license\" href=\"/COPYING\">GNU GPLv3</a> |
  Content is licensed under <a rel=\"license\" href=\"http://creativecommons.org/licenses/by-sa/4.0/\">CC-BY-SA</a> |
  <a href=\"/donate.html\">DONATE</a> |
  why the fuck is the &lt;marquee&gt; tag deprecated?
  </marquee>"
      org-html-postamble (format "<footer><hr>%s<hr>%s<hr>%s</footer>" marquee-shit webring marquee-shit))


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
                          ;;(format "<pre class=\"src src-%s\"%s>%s</pre>"
                          lang label code)))))))

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



(org-publish-remove-all-timestamps)
(org-publish-project "website")
