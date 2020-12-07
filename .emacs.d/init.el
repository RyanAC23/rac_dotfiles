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
 '(elfeed-feeds
   (quote
    ("https://www.youtube.com/feeds/videos.xml?channel_id=UCkqcY4CAuBFNFho6JgygCnA")))
 '(package-selected-packages
   (quote
    (lavender-theme ibuffer-vc mic-paren noflet maple-preview flycheck yasnippet-snippets yasnippet elfeed-org elfeed which-key web-mode use-package undo-tree try tabbar spaceline rainbow-mode projectile ox-reveal org-bullets htmlize esup emmet-mode diminish dashboard counsel company auto-complete all-the-icons)))
 '(tramp-terminal-type "dumb"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
