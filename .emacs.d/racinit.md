
# Table of Contents

1.  [Behavior](#org93bb8f9)
    1.  [UTF-8 Encoding](#orgfc4c142)
    2.  [Config Reload](#orgada4547)
2.  [SSH / TRAMP](#org923ee5a)
    1.  [Tramp](#org8841049)
3.  [Searching](#org1b4a32d)
4.  [Autocompletion](#org39c6ed3)
5.  [Navigation](#org24fbc41)
6.  [Dashboard / Homescreen](#orge96d2e9)
7.  [Org Mode](#org9f6ed7b)
8.  [Org Links Mode](#org34c5861)
9.  [C / C++](#orgcf89e76)
    1.  [Flycheck](#orgefbc309)
    2.  [Yasnippet](#orgd023368)
10. [Python](#org37c29d7)
11. [LaTeX](#org6218e45)
    1.  [Auctex / latexmk](#org7a692bc)
12. [Web Development](#org7e1f140)
13. [Website](#orgd321d5a)
14. [Elfeed (RSS Reader)](#org7d6d6bb)
15. [Theme and Appearance](#orge3a989a)
    1.  [Modeline](#orgf0afc02)
    2.  [diminish - hide minor modes from line](#orgd07232a)



<a id="org93bb8f9"></a>

# Behavior

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


<a id="orgfc4c142"></a>

## UTF-8 Encoding

    (setq locale-coding-system 'utf-8)
    (set-terminal-coding-system 'utf-8)
    (set-keyboard-coding-system 'utf-8)
    (set-selection-coding-system 'utf-8)
    (prefer-coding-system 'utf-8)


<a id="orgada4547"></a>

## Config Reload

For reloading your configuration file when editing on the fly.

    (defun config-reload ()
      "Reloads ~/.emacs.d/racinit.org when run."
      (interactive)
      (org-babel-load-file (expand-file-name "~/.emacs.d/racinit.org")))
    (global-set-key (kbd "<f5>") 'config-reload)


<a id="org923ee5a"></a>

# SSH / TRAMP


<a id="org8841049"></a>

## Tramp

    (setq tramp-verbose 10)


<a id="org1b4a32d"></a>

# Searching

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


<a id="org39c6ed3"></a>

# Autocompletion

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


<a id="org24fbc41"></a>

# Navigation

    ;; Navigation -------------------------------------------------------------
    ;; better buffer.
    (defalias 'list-buffers 'ibuffer)
    ;; Don't show filter groups if there are no filters in the group
    (setq ibuffer-show-empty-filter-groups nil)
    ;; Don't ask for confirmation to delete unmodified buffers
    (setq ibuffer-expert t)
    ;; Make ibuffer sort buffers
    ;; http://martinowen.net/blog/2010/02/03/tips-for-emacs-ibuffer.html

    ;; move between windows with shift+[arrow]
    ;; note: this will not work in org mode!
    (windmove-default-keybindings)

    (use-package undo-tree
      :ensure t
      :init
      (global-undo-tree-mode))


<a id="orge96d2e9"></a>

# Dashboard / Homescreen

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


<a id="org9f6ed7b"></a>

# Org Mode

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


<a id="org34c5861"></a>

# Org Links Mode

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


<a id="orgcf89e76"></a>

# DONE C / C++


<a id="orgefbc309"></a>

## Flycheck

      (use-package flycheck
        :ensure t
        :config
        (add-hook 'c-mode-hook 'flycheck-mode)
        (add-hook 'c++-mode-hook 'flycheck-mode)
    )


<a id="orgd023368"></a>

## Yasnippet

      (use-package yasnippet
        :ensure t
        :config
        (add-hook 'c-mode-hook 'yas-minor-mode)
        (add-hook 'c++-mode-hook 'yas-minor-mode)
    )

      (use-package yasnippet-snippets
        :ensure t)


<a id="org37c29d7"></a>

# TODO Python


<a id="org6218e45"></a>

# TODO LaTeX


<a id="org7a692bc"></a>

## Auctex / latexmk

     ;; (use-package auctex
     ;;     :ensure t)
    (use-package auctex-latexmk
         :ensure t)


<a id="org7e1f140"></a>

# TODO Web Development

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


<a id="orgd321d5a"></a>

# Website

    ;; If folders exist, load projects file
      (if (file-directory-p "~/Dropbox/share/orgpages/")
        (load "~/Dropbox/share/orgpages/pages.el"))


<a id="org7d6d6bb"></a>

# Elfeed (RSS Reader)


<a id="orge3a989a"></a>

# Theme and Appearance

    ;; Theme and Appearance ----------------------------------------------
    ;; free up space by killing the toolbar
    (tool-bar-mode -1)
    ;; Display clock and system load average
    (setq display-time-24hr-format t)
    (display-time-mode 1)

    ;; load a default theme.
    (load-theme 'deeper-blue t)

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


<a id="orgf0afc02"></a>

## Modeline

    (use-package spaceline
      :ensure t
      :config
      (require 'spaceline-config)
      (setq powerline-default-separator (quote arrow))
    (spaceline-spacemacs-theme))


<a id="orgd07232a"></a>

## diminish - hide minor modes from line

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