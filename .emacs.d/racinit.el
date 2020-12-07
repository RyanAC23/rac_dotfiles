;; [[file:~/repos/rac_dotfiles/.emacs.d/racinit.org::*Behavior][Behavior:1]]
;; Behavior -------------------------------------------------------------
;; enable common lisp syntax
(require 'cl-lib)

;; Disable the default splash screen.
(setq inhibit-splash-screen t)

;; Be able to type 'y' instead of 'yes'
(fset 'yes-or-no-p 'y-or-n-p)

;; delete trailing whitespace when saving
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; enable word wrap mode globally
(global-visual-line-mode)

;; reroute backups and control history
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

;; Start in server mode to open files in the server instance with the bash command
;; >>$ 'emacsclient [file]'
(server-start)

;; Enable line numbers by default. You might want to make this a local hook for certain filetypes.
(when (version<= "26.0.50" emacs-version )
  (global-display-line-numbers-mode))

;; Enable parenthesis matching mode
;; https://melpa.org/#/mic-paren
(use-package mic-paren
    :ensure t
    :config
    ;;(paren-activate)
    (add-hook 'c-mode-common-hook 'paren-activate)
    (add-hook 'python-mode-hook   'paren-activate)
)
;; Behavior:1 ends here

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
       (insert (format-time-string current-time-format (current-time)))
       (insert "\n")
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

;; [[file:~/repos/rac_dotfiles/.emacs.d/racinit.org::*Config%20Reload][Config Reload:1]]
(defun config-reload ()
  "Reloads ~/.emacs.d/racinit.org when run."
  (interactive)
  (org-babel-load-file (expand-file-name "~/.emacs.d/racinit.org")))
(global-set-key (kbd "<f5>") 'config-reload)
;; Config Reload:1 ends here

;; [[file:~/repos/rac_dotfiles/.emacs.d/racinit.org::*Tramp][Tramp:1]]
(setq tramp-verbose 10)
;; Tramp:1 ends here

;; [[file:~/repos/rac_dotfiles/.emacs.d/racinit.org::*Searching][Searching:1]]
;; Searching -----------------------------------------------------------
;; flexible pattern matching
;(setq ido-enable-flex-matching t)
;(setq ido-everywhere t)
;(ido-mode 1)

;; counsel is a requirement for swiper
(use-package counsel
  :ensure t
  :bind(("M-y" . counsel-yank-pop)
	:map ivy-minibuffer-map
	("M-y" . ivy-next-line)))

;; swiper is an improved search with intelligent pattern matching.
;; this makes ido-mode unecessary. Many of these rebindings are
;; probably unecessary as well.
(use-package swiper
  :ensure try
  :bind (("C-s" . swiper)
	 ("C-r" . swiper)
	 ("C-c C-r" . ivy-resume)
	 ("M-x" . counsel-M-x)
	 ("C-x C-f" . counsel-find-file))
  :config
  (progn
    (ivy-mode 1)
    (setq ivy-use-virtual-buffers t)
    (setq ivy-display-style 'fancy)
    (define-key read-expression-map (kbd "C-r") 'counsel-expression-history)))
;; Searching:1 ends here

;; [[file:~/repos/rac_dotfiles/.emacs.d/racinit.org::*Autocompletion][Autocompletion:1]]
;; Autocompletion ----------------------------------------------------------
;; We'll try company-mode for now. The old standard autocomplete was the
;; smartly named auto-complete, but only company is being actively developed.
(use-package company
  :ensure t
  :config
  (setq company-idle-delay 0)
  (setq company-minimum-prefix-length 3)
  :init
  (progn
    (global-company-mode t)))

;; C/C++ intellisense
;; may need clang compiler installed for this to work
;; (use-package company-irony
;;  :ensure t
;;  :config
;;  (require 'company)
;;  (add-to-list 'company-backends 'company-irony))

;; (use-package irony
;;  :ensure t
;;  :config
;;  (add-hook 'c++-mode-hook 'irony-mode)
;;  (add-hook 'c-mode-hook 'irony-mode)
;;  (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options))

;; (with-eval-after-load 'company
;;  (add-hook 'c++-mode-hook 'company-mode)
;;  (add-hook 'c-mode-hook 'company-mode))
;; Autocompletion:1 ends here

;; [[file:~/repos/rac_dotfiles/.emacs.d/racinit.org::*Navigation][Navigation:1]]
;; move between windows with shift+[arrow]
(windmove-default-keybindings)
;; Navigation:1 ends here

;; [[file:~/repos/rac_dotfiles/.emacs.d/racinit.org::*iBuffer][iBuffer:1]]
;; Navigation -------------------------------------------------------------
;; better buffer.
(defalias 'list-buffers 'ibuffer)
;; Don't show filter groups if there are no filters in the group
(setq ibuffer-show-empty-filter-groups nil)
(setq ibuffer-sorting-mode major-mode)
;; Don't ask for confirmation to delete unmodified buffers
(setq ibuffer-expert t)
;; Make ibuffer sort buffers
;; http://martinowen.net/blog/2010/02/03/tips-for-emacs-ibuffer.html
(use-package ibuffer-vc
  :ensure t
  :init (ibuffer-vc-set-filter-groups-by-vc-root))

(use-package undo-tree
  :ensure t
  :init
  (global-undo-tree-mode))
;; iBuffer:1 ends here

;; [[file:~/repos/rac_dotfiles/.emacs.d/racinit.org::*Dashboard%20/%20Homescreen][Dashboard / Homescreen:1]]
(use-package projectile
      :ensure t
      :init
      (projectile-mode 1))
  (global-set-key (kbd "C-c p") 'projectile-compile-project)

  (use-package all-the-icons
    :ensure t)
;; add install fonts if not present feature
(defun install-icon-fonts-checker (dir)
  (if ((file-exists-p dir) nil)
    (message "Not looking good, champ.")
    (message "Looks like it's there.")))
;; install if not present
(unless (file-exists-p "~/.local/share/fonts/all-the-icons.ttf")
  (all-the-icons-install-fonts))

    (use-package dashboard
	:ensure t
	:config
	  (dashboard-setup-startup-hook)
	  (setq dashboard-startup-banner "~/Dropbox/share/N23emacs/banners/banner.gif")
	  (setq dashboard-items '((recents . 5)
				  (projects . 5)
				  (bookmarks . 5)
				  (agenda . 5)
				  (registers . 5)))
	      ;; centering looks awful with multiple windows.
	      ;;(setq dashboard-center-content t)
	  (setq dashboard-set-file-icons t)
	  (setq dashboard-set-heading-icons t)
	  (setq dashboard-footer-messages nil)
	  (load-file "~/.emacs.d/dashboard_quotes.el")
	  (setq dashboard-banner-logo-title (nth (random (length dashboard-quote-list)) dashboard-quote-list)))
;; Dashboard / Homescreen:1 ends here

;; [[file:~/repos/rac_dotfiles/.emacs.d/racinit.org::*Org%20Mode][Org Mode:1]]
;; Org-mode ------------------------------------------------------------
(use-package org-bullets
  :ensure t
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

;; reveal.js presentations
(use-package ox-reveal
  :ensure ox-reveal)
;; We need to tell ox-reveal where to find the js file is.
;; https://github.com/yjwen/org-reveal#set-the-location-of-revealjs
(setq org-reveal-root "http://cdn.jsdelivr.net/npm/reveal.js")
(setq org-reveal-mathjax t)
;; enable syntax highlighting
(use-package htmlize
  :ensure t)

;; Add markdown export support
(require 'ox-md)
;; Org Mode:1 ends here

;; [[file:~/repos/rac_dotfiles/.emacs.d/racinit.org::*Org%20Links%20Mode][Org Links Mode:1]]
;; Org links mode [test] ---------------------------------------------------
  (global-set-key (kbd "C-c c")
		  'org-capture)
  (setq org-capture-templates
	'(
	  ("t" "To Do" entry (file+headline "~/Dropbox/share/N23emacs/todo-list.org" "Execute")
	  "* %?\n%T" :prepend t)
	  ("l" "Links" entry (file+headline "~/Dropbox/share/N23emacs/web-bookmarks.org" "Links")
	   "* %? %^L %^g \n%T" :prepend t)
	  ("w" "Links-Work" entry (file+headline "~/Dropbox/share/N23emacs/links-work.org" "Links")
	   "* %? %^L %^g \n%T" :prepend t)
))

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
  :ensure t )
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
	:ensure t
	:config
	(add-hook 'c-mode-hook 'flycheck-mode)
	(add-hook 'c-mode-hook '(setq flycheck-gcc-language-standard "gnu99"))
	(add-hook 'c++-mode-hook 'flycheck-mode)
	(add-hook 'python-mode-hook 'flycheck-mode)
)
;; Flycheck:1 ends here

;; [[file:~/repos/rac_dotfiles/.emacs.d/racinit.org::*Yasnippet][Yasnippet:1]]
(use-package yasnippet
    :ensure t
    :config
    (add-hook 'c-mode-hook 'yas-minor-mode)
    (add-hook 'c++-mode-hook 'yas-minor-mode)
)

  (use-package yasnippet-snippets
    :ensure t)
;; Yasnippet:1 ends here

;; [[file:~/repos/rac_dotfiles/.emacs.d/racinit.org::*Auctex%20/%20latexmk][Auctex / latexmk:1]]
;; (use-package auctex
 ;;     :ensure t)
(use-package auctex-latexmk
     :ensure t)
;; Auctex / latexmk:1 ends here

;; [[file:~/repos/rac_dotfiles/.emacs.d/racinit.org::*Web%20Development][Web Development:1]]
;; Web Development ---------------------------------------------------
(use-package web-mode
  :ensure t
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
  :ensure t
  :config
  (add-hook 'sgml-mode-hook 'emmet-mode) ;; Auto-start on any markup modes
  (add-hook 'web-mode-hook 'emmet-mode) ;; Auto-start on any markup modes
  (add-hook 'css-mode-hook 'emmet-mode) ;; enable Emmet's css abbreviation.
)
;; Web Development:1 ends here

;; [[file:~/repos/rac_dotfiles/.emacs.d/racinit.org::*Website][Website:1]]
;; If folders exist, load projects file
;; org test pages
(if (file-directory-p "~/Dropbox/share/orgpages/")
    (load "~/Dropbox/share/orgpages/pages.el"))
;; geocities website
(if (file-directory-p "~/repos/RyanAC23.github.io/")
    (load "~/repos/RyanAC23.github.io/resources/site-init.el"))
;; Website:1 ends here

;; [[file:~/repos/rac_dotfiles/.emacs.d/racinit.org::*Theme%20and%20Appearance][Theme and Appearance:1]]
;; Theme and Appearance ----------------------------------------------
;; free up space by killing the toolbar
(tool-bar-mode -1)
;; Display clock and system load average
(setq display-time-24hr-format t)
(display-time-mode 1)

;; load a default theme.
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
(load-theme 'neptune)
;;(load-theme './themes/neptune-theme.el)
;;(use-package lavender-theme
;;(use-package lavenderless-theme
;;    :ensure t)

;; Set transparency, and map transparency toggle to C-c t
;; from https://www.emacswiki.org/emacs/TransparentEmacs
(set-frame-parameter (selected-frame) 'alpha '(95 . 50))
(add-to-list 'default-frame-alist '(alpha . (95 . 50)))

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
;; Theme and Appearance:1 ends here

;; [[file:~/repos/rac_dotfiles/.emacs.d/racinit.org::*Modeline][Modeline:1]]
(use-package spaceline
  :ensure t
  :config
  (require 'spaceline-config)
  (setq powerline-default-separator (quote arrow))
(spaceline-spacemacs-theme))
;; Modeline:1 ends here

;; [[file:~/repos/rac_dotfiles/.emacs.d/racinit.org::*diminish%20-%20hide%20minor%20modes%20from%20line][diminish - hide minor modes from line:1]]
(use-package diminish
  :ensure t
  :init
  (diminish 'ivy-mode)
  (diminish 'page-break-lines-mode)
  (diminish 'undo-tree-mode)
  (diminish 'org-src-mode)
  (diminish 'which-key-mode)
  (diminish 'eldoc-mode)
  (diminish 'projectile-mode))
;; diminish - hide minor modes from line:1 ends here
