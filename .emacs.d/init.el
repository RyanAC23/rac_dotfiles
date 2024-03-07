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
 '(conda-anaconda-home "~/miniconda")
 '(custom-safe-themes
   '("21e57fbc8037e2126c36327df60295461584cc072cba669289d92602bbd4f5ce" "eed004f48332d418919f6e7ccf471748512f6ad40846ebbdde6bb18c0c7bb702" "ea9ff403246c27294a157e1ed00e5d60dc3d448f966470c2154ab593a794a286" "419e4e52df9d76bfdca3853b35f08750ec8b33594b4aaebbf2b8e79189e3bba0" "46021e31137aa6963cfc54b80b03328d8ae9cf9f22986a48f3da49a5e514cd95" "fc1be575480051a98b0df71f2602302ad6cba0aae8bf0eb23435d41c675ab8f8" "41d299ecdebddad8c0f9a54360be85f88d81d84520d3895c0b305c5fdf142d57" "754a64bc1ed03ecddd79bb59638c36adce4190a7ca24bdf374d409960916aec2" "dc7c3cb936ad634d6837241cc55716ff19074070ec345b87084774f5c96caf3a" "1632b73e6074cb253a6357e18bc04f0acd390ace77bdee0986ceaca78a5ca43c" default))
 '(elfeed-feeds 'nil)
 '(ignored-local-variable-values '((TeX-master . t)))
 '(org-agenda-files
   '("~/Dropbox/emacs/rac-agenda.org" "~/Dropbox/emacs/Birthdays.org"))
 '(org-export-headline-levels 10)
 '(package-selected-packages
   '(rainbow-mode neptune-light2-theme neptune-light-theme neptune-light elfeed-org org-ref-ivy org-ref lsp-treemacs lsp-ui lsp-mode conda eterm-256-color all-the-icons-dired magit ivy-rich dashboard-ls lavender-theme ibuffer-vc mic-paren noflet maple-preview which-key web-mode use-package undo-tree try tabbar spaceline projectile ox-reveal org-bullets htmlize esup diminish dashboard counsel company auto-complete all-the-icons yasnippet org-roam-ui org-roam lsp-ivy blacken))
 '(safe-local-variable-values '((TeX-master . t)))
 '(tramp-terminal-type "dumb"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-tooltip ((t (:family "Terminus")))))
(put 'downcase-region 'disabled nil)
