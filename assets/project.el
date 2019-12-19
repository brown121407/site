(require 'package)
(package-initialize)

(require 'ox)
(require 'f)

(setq posts-path "posts"
      pages-path "pages"
      assets-path "assets"
      publish-base-path "public"
      publish-posts-path (concat publish-base-path "/" posts-path)
      publish-assets-path (concat publish-base-path "/" assets-path))

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
	("website" :components ("pages" "posts" "assets"))))

(setq org-export-with-toc nil)
(setq org-html-head
      "<link rel='stylesheet' type='text/css' href='/assets/css/style.css'>")

(setq org-html-preamble "
<header>
    <nav>
        <a href='/'>HOME</a> |
        <a href='/donate.html'>DONATE</a> |
        <a href='/extra.html'>EXTRA</a>
    </nav>
    <hr>
</header>
")

(setq webring (f-read "includes/webring-out.html"))

(setq org-html-postamble (concat "
<footer>
    <hr>
    <marquee>
	<a href='https://git.sr.ht/~brown121407/brown.121407.xyz' target=\"_blank\">Source code</a> is licensed under <a rel=\"license\" href=\"/COPYING\">GNU GPLv3</a> | 
	Content is licensed under <a rel=\"license\" href=\"http://creativecommons.org/licenses/by-sa/4.0/\">CC-BY-SA</a> |
	<a href=\"/donate.html\">DONATE</a> |
	    why the fuck is the &lt;marquee&gt; tag deprecated?
    </marquee>
    <hr>
"
				 webring
				 "<hr>
<marquee>
	<a href='https://git.sr.ht/~brown121407/brown.121407.xyz' target=\"_blank\">Source code</a> is licensed under <a rel=\"license\" href=\"/COPYING\">GNU GPLv3</a> | 
	Content is licensed under <a rel=\"license\" href=\"http://creativecommons.org/licenses/by-sa/4.0/\">CC-BY-SA</a> |
	<a href=\"/donate.html\">DONATE</a> |
	    why the fuck is the &lt;marquee&gt; tag deprecated?
    </marquee>
</footer>"))


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


;; TODO find a way to maybe avoid publishing two times?
(org-publish-remove-all-timestamps)
(org-publish-project "website")

(let* ((file-paths (f-files posts-path))
       (posts (mapcar
	       (lambda (path)
		 (list (f-filename path)
		       (org-publish-find-title path '("posts"))
		       (format-time-string "%Y-%m-%d"
					   (org-publish-find-date path '("posts")))))
	       file-paths))
       (lines (mapcar
	       (lambda (x)
		 (format "- %s - [[../posts/%s][%s]]\n"
			 (caddr x) (car x) (cadr x)))
	       posts)))
  (f-delete "includes/posts.inc")
  (f-write "" 'utf-8 "includes/posts.inc")
  (dolist (line (sort lines 'string>))
    (f-append line 'utf-8 "includes/posts.inc")))

(org-publish-project "website")
