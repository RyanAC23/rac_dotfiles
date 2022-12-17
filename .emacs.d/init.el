;; Emacs init file [./.emacs.d/init.el]

;; ----- Packages --------------------------------------------------------------
(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/"))

(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

;; ----- load external init file ---------------------------------------------------------

;; loading external config files
(defun load-if-exists (f)
  "Load the specified file if it exists."
  (if (file-readable-p f)
      (load-file (expand-file-name f))
    (message "No file to load was found.")
    )
  )

(load-if-exists "~/.emacs.d/racinit.el")

;; ----- Auto Set Variables -----------------------------------------------------

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(bmkp-last-as-first-bookmark-file "~/.emacs.d/bookmarks")
 '(conda-anaconda-home "~/apps/miniconda")
 '(custom-safe-themes
   '("754a64bc1ed03ecddd79bb59638c36adce4190a7ca24bdf374d409960916aec2" "dc7c3cb936ad634d6837241cc55716ff19074070ec345b87084774f5c96caf3a" "1632b73e6074cb253a6357e18bc04f0acd390ace77bdee0986ceaca78a5ca43c" default))
 '(elfeed-feeds 'nil)
 '(org-agenda-files
   '("~/Dropbox/emacs/rac-agenda.org" "~/Dropbox/emacs/Birthdays.org"))
 '(org-export-headline-levels 10)
 '(package-selected-packages
   '(org-ref-ivy org-ref lsp-treemacs lsp-ui lsp-mode conda eterm-256-color all-the-icons-dired magit ivy-rich tex auctex-latexmk-setup dashboard-ls lavender-theme ibuffer-vc mic-paren noflet maple-preview which-key web-mode use-package undo-tree try tabbar spaceline projectile ox-reveal org-bullets htmlize esup diminish dashboard counsel company auto-complete all-the-icons yasnippet org-roam-ui org-roam lsp-ivy blacken))
 '(tramp-terminal-type "dumb")
 '(warning-suppress-log-types '((auto-save) (use-package)))
 '(warning-suppress-types '((auto-save) (use-package))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-tooltip ((t (:family "Terminus")))))
(put 'downcase-region 'disabled nil)
