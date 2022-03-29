  (require 'ox-publish)
  (setq org-publish-project-alist
	'(

	  ;; ... add all the components here (see below)...
	  ("RyanAC23-website" :components ("website-notes" "website-static"))

	  ("website-notes"
	   :base-directory "~/Dropbox/website/org/"
	   :base-extension "org"
	   :publishing-directory "~/Dropbox/website/public_html/"
	   :recursive t
	   :publishing-function org-html-publish-to-html
	   :headline-levels 4
	   :auto-preamble t
	   )

	  ("website-static"
	   :base-directory "~/Dropbox/website/org/"
	   :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf\\|html"
	   :publishing-directory "~/Dropbox/website/public_html/"
	   :recursive t
	   :publishing-function org-publish-attachment
	   )
	  ))
