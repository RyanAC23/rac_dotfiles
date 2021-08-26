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
(use-package which-key
  :ensure t
  :config (which-key-mode))

;; load org init file ------------------------------------------------------
(org-babel-load-file (expand-file-name "~/.emacs.d/racinit.org"))

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
    ("https://www.youtube.com/feeds/videos.xml?channel_id=UCkqcY4CAuBFNFho6JgygCnA")))
 '(org-capture-templates
   (quote
    (("t" "To Do" entry
      (file+headline "~/Dropbox/share/N23emacs/todo-list.org" "Execute")
      "* %?
%T" :prepend t)
     ("l" "Links" entry
      (file+headline "~/Dropbox/share/emacs/org/capture/links-personal.org" "Links")
      "* %? %^L %^g
%T" :prepend t)
     ("w" "Links-Work" entry
      (file+headline "~/Dropbox/share/emacs/org/capture/links-work.org" "Links")
      "* %? %^L %^g
%T" :prepend t))))
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
