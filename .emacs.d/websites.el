(require 'ox-publish)

(setq org-publish-project-alist
      '(
        ;; Add all site components here.
        ("Neppermint-website" :components ("geocite"
					   "7D76_671B"
					   "site-capture"
					   "website-static"))
        ("geocite"
         :base-directory "~/Dropbox/website/org/geocite/"
         :base-extension "org"
         :publishing-directory "~/Dropbox/website/public_html/geocite/"
         :recursive t
         :publishing-function org-html-publish-to-html
         :headline-levels 10
         :auto-preamble t)
        ("7D76_671B"
         :base-directory "~/Dropbox/website/org/7D76_671B/"
         :base-extension "org"
         :publishing-directory "~/Dropbox/website/public_html/7D76_671B/"
         :recursive t
         :publishing-function org-html-publish-to-html
         :headline-levels 10
         :auto-preamble t)
        ("site-capture"
         :base-directory "~/Dropbox/website/org/capture/"
         :base-extension "org"
         :publishing-directory "~/Dropbox/website/public_html/capture/"
         :recursive t
         :publishing-function org-html-publish-to-html
         :headline-levels 10
         :auto-preamble t)
        ("website-static"
         :base-directory "~/Dropbox/website/org/"
         :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf\\|html"
         :publishing-directory "~/Dropbox/website/public_html/"
         :recursive t
         :publishing-function org-publish-attachment)))
