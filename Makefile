mkfile_path := $(abspath $(lastword $(MAKEFILE_LIST)))
mkfile_dir := $(dir $(mkfile_path))

.PHONY : publish clean

all : clean ring publish

publish : clean
	emacs --script project.el

ring :
	assets/bin/openring \
	  -s https://drewdevault.com/feed.xml \
          -s https://emersion.fr/blog/rss.xml \
          -s https://danluu.com/atom.xml \
          -s https://www.fsf.org/static/fsforg/rss/news.xml \
          -s https://www.fsf.org/static/fsforg/rss/blogs.xml \
          -n 6 \
          < includes/webring-in.html \
          > includes/webring-out.html

clean :
	rm -rf ./public
