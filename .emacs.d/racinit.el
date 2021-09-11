;; [[file:~/repos/rac_dotfiles/.emacs.d/racinit.org::*Packages][Packages:1]]
(require 'use-package-ensure)
(setq use-package-always-ensure t)
;; Packages:1 ends here

;; [[file:~/repos/rac_dotfiles/.emacs.d/racinit.org::*Common%20Lisp][Common Lisp:1]]
(require 'cl-lib)
;; Common Lisp:1 ends here

;; [[file:~/repos/rac_dotfiles/.emacs.d/racinit.org::*y/n%20instead%20of%20'yes/no'][y/n instead of 'yes/no':1]]
(fset 'yes-or-no-p 'y-or-n-p)
;; y/n instead of 'yes/no':1 ends here

;; [[file:~/repos/rac_dotfiles/.emacs.d/racinit.org::*which-key%20mode][which-key mode:1]]
(use-package which-key
  :diminish which-key-mode
  :config (which-key-mode))
;; which-key mode:1 ends here

;; [[file:~/repos/rac_dotfiles/.emacs.d/racinit.org::*No%20Trailing%20Whitespace][No Trailing Whitespace:1]]
(add-hook 'before-save-hook 'delete-trailing-whitespace)
;; No Trailing Whitespace:1 ends here

;; [[file:~/repos/rac_dotfiles/.emacs.d/racinit.org::*Word%20Wrap][Word Wrap:1]]
(global-visual-line-mode)
;; Word Wrap:1 ends here

;; [[file:~/repos/rac_dotfiles/.emacs.d/racinit.org::*Cleaner%20Directories][Cleaner Directories:1]]
(defvar --backup-directory (concat user-emacs-directory "backups"))
(if (not (file-exists-p --backup-directory))
	(make-directory --backup-directory t))
(setq backup-directory-alist `(("." . ,--backup-directory)))
(setq make-backup-files t          ; backup of a file the first time it is saved.
      backup-by-copying t          ; don't clobber symlinks
      version-control t            ; version numbers for backup files
      delete-old-versions t        ; delete excess backup files silently
      delete-by-moving-to-trash t
      kept-old-versions 6          ; oldest versions to keep when a new numbered backup is made (default: 2)
      kept-new-versions 9          ; newest versions to keep when a new numbered backup is made (default: 2)
      auto-save-default t          ; auto-save every buffer that visits a file
      auto-save-timeout 20         ; number of seconds idle time before auto-save (default: 30)
      auto-save-interval 200       ; number of keystrokes between auto-saves (default: 300)
      )
;; Cleaner Directories:1 ends here

;; [[file:~/repos/rac_dotfiles/.emacs.d/racinit.org::*Server%20Mode][Server Mode:1]]
(require 'server)
(unless (server-running-p)
  (progn
    (server-start)
    (toggle-frame-maximized)
    )
)
;; Server Mode:1 ends here

;; [[file:~/repos/rac_dotfiles/.emacs.d/racinit.org::*Line%20Numbers][Line Numbers:1]]
(when (version<= "26.0.50" emacs-version )
  (global-display-line-numbers-mode))
;; Line Numbers:1 ends here

;; [[file:~/repos/rac_dotfiles/.emacs.d/racinit.org::*Enable%20parenthesis%20matching%20mode][Enable parenthesis matching mode:1]]
(use-package mic-paren
    :config
    ;;(paren-activate)
    (add-hook 'c-mode-common-hook 'paren-activate)
    (add-hook 'python-mode-hook   'paren-activate)
    (add-hook 'org-mode-hook      'paren-activate)
)
;; Enable parenthesis matching mode:1 ends here

;; [[file:~/repos/rac_dotfiles/.emacs.d/racinit.org::*Navigation][Navigation:1]]
;; move between windows with shift+[arrow]
    (windmove-default-keybindings)
;; Navigation:1 ends here

;; [[file:~/repos/rac_dotfiles/.emacs.d/racinit.org::*Quick%20Reload%20init.el][Quick Reload init.el:1]]
(defun reload-init-file ()
  (interactive)
  (load-file user-init-file))

(global-set-key (kbd "C-c r") 'reload-init-file)
;; Quick Reload init.el:1 ends here

;; [[file:~/repos/rac_dotfiles/.emacs.d/racinit.org::*Insert%20timestamp][Insert timestamp:1]]
;; ====================
;; insert date and time

(defvar current-date-time-format "%a %b %d %H:%M:%S %Z %Y"
  "Format of date to insert with `insert-current-date-time' func
See help of `format-time-string' for possible replacements")

(defvar current-time-format "%a %H:%M:%S"
  "Format of date to insert with `insert-current-time' func.
Note the weekly scope of the command's precision.")

(defun insert-current-date-time ()
  "insert the current date and time into current buffer.
Uses `current-date-time-format' for the formatting the date/time."
       (interactive)
       (insert "==========\n")
;       (insert (let () (comment-start)))
       (insert (format-time-string current-date-time-format (current-time)))
       (insert "\n")
       )

(defun insert-current-time ()
  "insert the current time (1-week scope) into the current buffer."
       (interactive)
       (insert "- ")
       (insert (format-time-string current-time-format (current-time)))
       (insert " ")
       )

(global-set-key "\C-x\C-d" 'insert-current-date-time)
(global-set-key "\C-x\C-t" 'insert-current-time)
;; Insert timestamp:1 ends here

;; [[file:~/repos/rac_dotfiles/.emacs.d/racinit.org::*UTF-8%20Encoding][UTF-8 Encoding:1]]
(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
;; UTF-8 Encoding:1 ends here

;; [[file:~/repos/rac_dotfiles/.emacs.d/racinit.org::*Try][Try:1]]
(use-package try)
;; Try:1 ends here

;; [[file:~/repos/rac_dotfiles/.emacs.d/racinit.org::*Visual%20Tweaks][Visual Tweaks:1]]
(setq inhibit-splash-screen t)
(scroll-bar-mode -1)
(menu-bar-mode -1)
(tool-bar-mode -1)
;; Visual Tweaks:1 ends here

;; [[file:~/repos/rac_dotfiles/.emacs.d/racinit.org::*Display%20clock%20and%20system%20load%20average][Display clock and system load average:1]]
(setq display-time-24hr-format t)
(display-time-mode 1)
;; Display clock and system load average:1 ends here

;; [[file:~/repos/rac_dotfiles/.emacs.d/racinit.org::*load%20a%20default%20theme.][load a default theme.:1]]
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
(if (display-graphic-p)
    (load-theme 'neptune t))
;; load a default theme.:1 ends here

;; [[file:~/repos/rac_dotfiles/.emacs.d/racinit.org::*Transparency][Transparency:1]]
(defun toggle-transparency ()
   (interactive)
   (let ((alpha (frame-parameter nil 'alpha)))
     (set-frame-parameter
      nil 'alpha
      (if (eql (cond ((numberp alpha) alpha)
                     ((numberp (cdr alpha)) (cdr alpha))
                     ;; Also handle undocumented (<active> <inactive>) form.
                     ((numberp (cadr alpha)) (cadr alpha)))
               100)
          '(95 . 50) '(100 . 100)))))
(global-set-key (kbd "C-c t") 'toggle-transparency)
;; Transparency:1 ends here

;; [[file:~/repos/rac_dotfiles/.emacs.d/racinit.org::*Modeline][Modeline:1]]
(use-package spaceline
  :config
  (require 'spaceline-config)
  (setq powerline-default-separator (quote arrow))
(spaceline-spacemacs-theme))
;; Modeline:1 ends here

;; [[file:~/repos/rac_dotfiles/.emacs.d/racinit.org::*diminish%20-%20hide%20minor%20modes%20from%20line][diminish - hide minor modes from line:1]]
(use-package diminish
  :init
  (diminish 'page-break-lines-mode)
  (diminish 'undo-tree-mode)
  (diminish 'org-src-mode)
  (diminish 'eldoc-mode))
;; diminish - hide minor modes from line:1 ends here

;; [[file:~/repos/rac_dotfiles/.emacs.d/racinit.org::*Tramp][Tramp:1]]
(setq tramp-verbose 10)
;; Tramp:1 ends here

;; [[file:~/repos/rac_dotfiles/.emacs.d/racinit.org::*Version%20Control][Version Control:1]]
(use-package magit
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1)
  )
;; Version Control:1 ends here

;; [[file:~/repos/rac_dotfiles/.emacs.d/racinit.org::*Searching][Searching:1]]
;; ivy gives intelligent file search with M-x
(use-package ivy
  :diminish
  :config
  (ivy-mode 1)
)

;; counsel is a requirement for swiper
(use-package counsel)

;; swiper is an improved search with intelligent pattern matching.
(use-package swiper
  :bind (("C-s" . swiper)
	 ("C-r" . swiper)
	 ("C-c C-r" . ivy-resume)
	 ("M-x" . counsel-M-x)
	 ("C-x C-f" . counsel-find-file)
	 ("M-y" . counsel-yank-pop)
	 ("M-n" . (lambda () (interactive) (search-forward (car swiper-history))))
	 ("M-p" . (lambda () (interactive) (search-backward (car swiper-history))))
	 )
  :config
  (progn
    (setq ivy-use-virtual-buffers t)
    (setq ivy-display-style 'fancy)
    (define-key read-expression-map (kbd "C-r") 'counsel-expression-history))
    )

(use-package ivy-rich
:init
(ivy-rich-mode 1))
;; Searching:1 ends here

;; [[file:~/repos/rac_dotfiles/.emacs.d/racinit.org::*Autocompletion][Autocompletion:1]]
;; Autocompletion ----------------------------------------------------------
;; We'll try company-mode for now. The old standard autocomplete was the
;; smartly named auto-complete, but only company is being actively developed.
 (use-package company
   :init
   (add-hook 'emacs-lisp-mode-hook 'company-mode)
   (add-hook 'org-mode-hook 'company-mode)
   (add-hook 'c++-mode-hook 'company-mode)
   (add-hook 'c-mode-hook 'company-mode))

;; C/C++ intellisense
;; may need clang compiler installed for this to work
;; (use-package company-irony
;;  :config
;;  (require 'company)
;;  (add-to-list 'company-backends 'company-irony))

;; (use-package irony
;;  :config
;;  (add-hook 'c++-mode-hook 'irony-mode)
;;  (add-hook 'c-mode-hook 'irony-mode)
;;  (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options))
;; Autocompletion:1 ends here

;; [[file:~/repos/rac_dotfiles/.emacs.d/racinit.org::*iBuffer][iBuffer:1]]
;; Navigation -------------------------------------------------------------
(defalias 'list-buffers 'ibuffer)
;; Don't show filter groups if there are no filters in the group
(setq ibuffer-show-empty-filter-groups nil)
(setq ibuffer-sorting-mode major-mode)
;; Don't ask for confirmation to delete unmodified buffers
(setq ibuffer-expert t)

;; categorize buffers by groups:
(setq ibuffer-saved-filter-groups
      (quote (("default"
	       ("python" (mode . python-mode))
	       ("c/c++" (or
			 (mode . c-mode)
			 (mode . c++-mode)))
	       ("org"
		         (mode . org-mode))
	       ("web"
			 (or
			 (mode . web-mode)
			 (mode . css-mode)))
	       ("emacs" (or
			 (name . "^\\*scratch\\*$")
			 (name . "^\\*Messages\\*$")))))))

(add-hook 'ibuffer-mode-hook
	  (lambda ()
	    (ibuffer-switch-to-saved-filter-groups "default")))
;; iBuffer:1 ends here

;; [[file:~/repos/rac_dotfiles/.emacs.d/racinit.org::*Dashboard%20/%20Homescreen][Dashboard / Homescreen:1]]
(use-package projectile
     :diminish projectile-mode
     :config (projectile-mode)
     :bind-keymap
     ("C-c p" . projectile-command-map)
     :custom ((projectile-completion-system 'ivy))
     :init
     (when (file-directory-p "~/repos/")
       (setq projectile-project-search-path '("~/repos/")))
     )

   (use-package all-the-icons
)
   ;; add install fonts if not present feature
   ;; (defun install-icon-fonts-checker (dir)
   ;;   (if ((file-exists-p dir) nil)
   ;;       (message "Not looking good, champ.")
   ;;     (message "Looks like it's there.")))
   ;; install if not present
   (unless (file-exists-p "~/.local/share/fonts/all-the-icons.ttf")
     (all-the-icons-install-fonts))

  (use-package dashboard
    :config
    (dashboard-setup-startup-hook)
    (setq dashboard-startup-banner "~/.emacs.d/banner/banner.gif")
    (setq dashboard-items '((recents . 15)
			     (projects . 5)
			     (bookmarks . 5)
			     (agenda . 5)
			     (registers . 5)))
    ;; centering looks awful with multiple frames.
    (setq dashboard-center-content t)
    (setq dashboard-set-file-icons t)
    (setq dashboard-set-heading-icons t)
    (setq dashboard-footer-messages nil)
    (load-file "~/.emacs.d/dashboard_quotes.el")
    (setq dashboard-banner-logo-title (nth (random (length dashboard-quote-list)) dashboard-quote-list)))
;; Dashboard / Homescreen:1 ends here

;; [[file:~/repos/rac_dotfiles/.emacs.d/racinit.org::*Org%20Mode][Org Mode:1]]
;; Org-mode ------------------------------------------------------------
  (use-package org
    :config
;;    (setq org-ellipsis " .")
    )

  (use-package org-bullets
    :config
    (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

  ;; reveal.js presentations
  (use-package ox-reveal
    :ensure ox-reveal)
  ;; We need to tell ox-reveal where to find the js file.
  ;; https://github.com/yjwen/org-reveal#set-the-location-of-revealjs
  (setq org-reveal-root "http://cdn.jsdelivr.net/npm/reveal.js")
  (setq org-reveal-mathjax t)
  ;; enable syntax highlighting
  (use-package htmlize
  )

  ;; Add markdown export support
  (require 'ox-md)
;; Org Mode:1 ends here

;; [[file:~/repos/rac_dotfiles/.emacs.d/racinit.org::*Org%20Links%20Mode][Org Links Mode:1]]
(global-set-key (kbd "C-c c")
		  'org-capture)

  (defadvice org-capture-finalize
      (after delete-capture-frame activate)
    "Advise capture-finalize to close the frame"
    (if (equal "capture" (frame-parameter nil 'name))
	(delete-frame)))

  (defadvice org-capture-destroy
      (after delete-capture-frame activate)
    "Advise capture-destroy to close the frame"
    (if (equal "capture" (frame-parameter nil 'name))
	(delete-frame)))

  (use-package noflet
)

  (defun make-capture-frame ()
    "Create a new frame and run org-capture."
    (interactive)
    (make-frame '((name . "capture")))
    (select-frame-by-name "capture")
    (delete-other-windows)
    (noflet ((switch-to-buffer-other-window (buf) (switch-to-buffer buf)))
      (org-capture)))
;; Org Links Mode:1 ends here

;; [[file:~/repos/rac_dotfiles/.emacs.d/racinit.org::*Flycheck][Flycheck:1]]
(use-package flycheck
  :config
    (add-hook 'c-mode-hook 'flycheck-mode)
    (add-hook 'c-mode-hook '(lambda () (setq flycheck-gcc-language-standard "gnu99")))
    (add-hook 'c++-mode-hook 'flycheck-mode)
    ;;(add-hook 'python-mode-hook 'flycheck-mode)
    )
;; Flycheck:1 ends here

;; [[file:~/repos/rac_dotfiles/.emacs.d/racinit.org::*Yasnippet][Yasnippet:1]]
(use-package yasnippet
    :config
    (add-hook 'c-mode-hook 'yas-minor-mode)
    (add-hook 'c++-mode-hook 'yas-minor-mode)
    ;;(add-hook 'python-mode-hook 'yas-minor-mode)
  )
  (use-package yasnippet-snippets
)
;; Yasnippet:1 ends here

;; [[file:~/repos/rac_dotfiles/.emacs.d/racinit.org::*Blacken%20Hook][Blacken Hook:1]]
(use-package blacken
    :config
    (add-hook 'python-mode-hook 'blacken-mode)
)
;; Blacken Hook:1 ends here

;; [[file:~/repos/rac_dotfiles/.emacs.d/racinit.org::*Selective%20Display][Selective Display:1]]
(defun indent-show-all ()
    (interactive)
    (set-selective-display nil)
    (condition-case nil (hs-show-all) (error nil))
    (show-all))
  (defun python-remap-fs ()
    (global-set-key [f1] 'indent-show-all)
    (global-set-key [f2] (lambda () (interactive) (set-selective-display
						 standard-indent)))
    (global-set-key [f3] (lambda () (interactive) (set-selective-display
						 (* 2 standard-indent))))
    (global-set-key [f4] (lambda () (interactive) (set-selective-display
						 (* 3 standard-indent))))
    (global-set-key [f5] (lambda () (interactive) (set-selective-display
						 (* 4 standard-indent))))
    (global-set-key [f6] (lambda () (interactive) (set-selective-display
						 (* 5 standard-indent))))
    (global-set-key [f7] (lambda () (interactive) (set-selective-display
						 nil)))
  )

(add-hook 'python-mode-hook 'python-remap-fs)
;; Selective Display:1 ends here

;; [[file:~/repos/rac_dotfiles/.emacs.d/racinit.org::*Auctex%20/%20latexmk][Auctex / latexmk:1]]
(use-package tex
    :ensure auctex
)
(use-package auctex-latexmk
)
(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq-default TeX-master nil)
(add-hook 'LaTeX-mode-hook 'visual-line-mode)
(add-hook 'LaTeX-mode-hook 'flyspell-mode)
(add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)
(setq reftex-plug-into-AUCTeX t)
;; Auctex / latexmk:1 ends here

;; [[file:~/repos/rac_dotfiles/.emacs.d/racinit.org::*Web%20Development][Web Development:1]]
(use-package web-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
  (setq web-mode-engines-alist
	'(("django" . "\\.html\\'")))
  (setq web-mode-ac-sources-alist
	'(("css" . (ac-source-css-property))
	  ("html" . (ac-source-words-in-buffer ac-source-abbrev))))
  (setq web-mode-enable-auto-closing t)
  (setq web-mode-enable-auto-quoting t)
  (setq web-mode-enable-current-column-highlight t)
  (setq web-mode-enable-current-element-highlight t))

(use-package emmet-mode
  :config
  (add-hook 'sgml-mode-hook 'emmet-mode) ;; Auto-start on any markup modes
  (add-hook 'web-mode-hook 'emmet-mode) ;; Auto-start on any markup modes
  (add-hook 'css-mode-hook 'emmet-mode) ;; enable Emmet's css abbreviation.
)
;; Web Development:1 ends here

;; [[file:~/repos/rac_dotfiles/.emacs.d/racinit.org::*Main%20Website%20Export][Main Website Export:1]]
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
;; Main Website Export:1 ends here

;; [[file:~/repos/rac_dotfiles/.emacs.d/racinit.org::*Rebuild%20Sites][Rebuild Sites:1]]
(global-set-key (kbd "C-c b") 'org-publish-project)
;; Rebuild Sites:1 ends here

;; [[file:~/repos/rac_dotfiles/.emacs.d/racinit.org::*Custom%20Commands][Custom Commands:1]]
(add-to-list 'org-src-lang-modes '("inline-js" . javascript))
(defvar org-babel-default-header-args:inline-js
  '((:results . "html")
    (:exports . "results")))
(defun org-babel-execute:inline-js (body _params)
  (format "<script type=\"text/javascript\">\n%s\n</script>" body))
;; Custom Commands:1 ends here

;; [[file:~/repos/rac_dotfiles/.emacs.d/racinit.org::*RSS%20-%20Elfeed][RSS - Elfeed:1]]
(use-package elfeed
    )
  (global-set-key (kbd "C-x w") 'elfeed)
  (setq-default elfeed-search-filter "@2-months-ago")
  (add-hook 'emacs-startup-hook (lambda () (run-at-time 5 5 'elfeed-update)))


(let ((elfeed-urls "~/Dropbox/emacs/rac_elfeeds.el"))
 (when (file-exists-p elfeed-urls)
   (load-file elfeed-urls))
)
;; RSS - Elfeed:1 ends here
