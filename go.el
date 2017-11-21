(require 'ox-publish)

(setq org-export-with-sub-superscripts nil)
(setq org-publish-project-alist
      '(
	("stats"
	 :base-directory "org/"
	 :base-extension "org"
	 :publishing-directory "~/github/stats/html"
	 :publishing-function org-html-publish-to-html
	 :recursive t
	 :htmlized-source t
	 :section-numbers nil
         :html-postamble "<hr />%a | %d<br>This <span xmlns:dct=\"http://purl.org/dc/terms/\" href=\"http://purl.org/dc/dcmitype/Text\" rel=\"dct:type\">work</span> is licensed under a <a rel=\"license\" href=\"http://creativecommons.org/licenses/by/4.0/\">Creative Commons Attribution 4.0 International License</a><br><a rel=\"license\"href=\"http://creativecommons.org/licenses/by/4.0/\"><img alt=\"Creative Commons License\" style=\"border-width:0\" src=\"http://i.creativecommons.org/l/by/4.0/80x15.png\" /></a><br />"
	 :language en
	 :html-link-home "index.html"
	 :html-link-up "index.html"
	 :html-head "<link rel=\"stylesheet\" type=\"text/css\" href=\"mystyle.css\" /><script>(function(i,s,o,g,r,a,m){i['GoogleAnalyticsObject']=r;i[r]=i[r]||function(){(i[r].q=i[r].q||[]).push(arguments)},i[r].l=1*new Date();a=s.createElement(o),m=s.getElementsByTagName(o)[0];a.async=1;a.src=g;m.parentNode.insertBefore(a,m)})(window,document,'script','//www.google-analytics.com/analytics.js','ga');ga('create', 'UA-52544521-1', 'auto');ga('send', 'pageview');</script>"
	 )
	("stats_html"
	 :base-directory "html/"
	 :base-extension "css\\|html"
	 :publishing-directory "/ssh:plg@abby.gribblelab.org:~/gribblelab.org/stats/"
	 :publishing-function org-publish-attachment
	 :recursive t
	 )
	("stats_code"
	 :base-directory "code/"
	 :base-extension "c\\|h\\|txt\\|csv\\|r\\|R"
	 :publishing-directory "/ssh:plg@abby.gribblelab.org:~/gribblelab.org/stats/code/"
	 :publishing-function org-publish-attachment
	 :recursive t
	 )
	("stats_data"
	 :base-directory "data/"
	 :base-extension "c\\|h\\|txt\\|csv\\|r\\|R"
	 :publishing-directory "/ssh:plg@abby.gribblelab.org:~/gribblelab.org/stats/data/"
	 :publishing-function org-publish-attachment
	 :recursive t
	 )
	("stats_slides"
	 :base-directory "slides/"
	 :base-extension "pdf"
	 :publishing-directory "/ssh:plg@abby.gribblelab.org:~/gribblelab.org/stats/slides/"
	 :publishing-function org-publish-attachment
	 :recursive t
	 )
	("stats_notes"
	 :base-directory "notes/"
	 :base-extension "html\\|Rmd\\|pdf\\|Rnw\\|bib"
	 :publishing-directory "/ssh:plg@abby.gribblelab.org:~/gribblelab.org/stats/notes/"
	 :publishing-function org-publish-attachment
	 :recursive t
	 )
	("stats_assignments"
	 :base-directory "assignments/"
	 :base-extension "tex\\|pdf"
	 :publishing-directory "/ssh:plg@abby.gribblelab.org:~/gribblelab.org/stats/assignments/"
	 :publishing-function org-publish-attachment
	 :recursive t
	 )
	("org" :components ("stats" "stats_html" "stats_code" "stats_data" "stats_images" "stats_slides" "stats_notes" "stats_assignments"))))

(org-publish-project "stats")
(org-publish-project "stats_html")
(org-publish-project "stats_code")
(org-publish-project "stats_data")
(org-publish-project "stats_slides")
(org-publish-project "stats_notes")
(org-publish-project "stats_assignments")
