;; Emacs init file [./.emacs.d/init.el]

;; loading external config files
(defun load-if-exists (f)
  "Load the specified file if it exists."
  (if (file-readable-p f)
      (load-file f)))
(load-if-exists "[filename here]")

;; Packages -------------------------------------------------------------
(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/"))

(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(use-package try
  :ensure t)

;; load org init file ------------------------------------------------------
(org-babel-load-file (expand-file-name "~/.emacs.d/racinit.org"))

(setq org-capture-templates
      '(
	("k" "Links-kabal" entry (file+headline "~/Dropbox/website/org/capture/links-kabal.org" "Links")
	 "* %? %^L %^g \n%T" :prepend t)
	("l" "Links-general" entry (file+headline "~/Dropbox/website/org/capture/links-general.org" "Links")
	 "* %? %^L %^g \n%T" :prepend t)
	("w" "Links-work" entry (file+headline "~/Dropbox/website/org/capture/links-work.org" "Links")
	 "* %? %^L %^g \n%T" :prepend t)
	))



(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("dc7c3cb936ad634d6837241cc55716ff19074070ec345b87084774f5c96caf3a" "1632b73e6074cb253a6357e18bc04f0acd390ace77bdee0986ceaca78a5ca43c" default)))
 '(elfeed-feeds
   (quote
    ("https://mtracey.substack.com/feed" "http://ronpaulinstitute.org/archives/peace-and-prosperity/rss.aspx?blogid=5" "https://alexberenson.substack.com/feed" "https://www.youtube.com/feeds/videos.xml?channel_id=UCkqcY4CAuBFNFho6JgygCnA")))
 '(package-selected-packages
   (quote
    (tex auctex-latexmk-setup treemacs-all-the-icons treemacs-projectile treemacs dashboard-ls lavender-theme ibuffer-vc mic-paren noflet maple-preview flycheck yasnippet-snippets yasnippet elfeed-org elfeed which-key web-mode use-package undo-tree try tabbar spaceline rainbow-mode projectile ox-reveal org-bullets htmlize esup emmet-mode diminish dashboard counsel company auto-complete all-the-icons)))
 '(tramp-terminal-type "dumb"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
