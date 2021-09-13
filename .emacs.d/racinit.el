;; The default is 800 kilobytes.  Measured in bytes.
  ;;(setq gc-cons-threshold (* 50 1000 1000))

  ;; Profile emacs startup

  (defun run-startup-diagnostics ()
    (add-hook 'emacs-startup-hook
              (lambda ()
                (message "*** Emacs loaded in %s with %d garbage collections."
                         (format "%.2f seconds"
                                 (float-time
                                  (time-subtract after-init-time before-init-time)))
                         gcs-done)))
    (setq use-package-verbose t)
    )

(run-startup-diagnostics)

(require 'use-package-ensure)
(setq use-package-always-ensure t)

(require 'cl-lib)

(fset 'yes-or-no-p 'y-or-n-p)

(use-package which-key
  :defer 0
  :diminish which-key-mode
  :config
  (which-key-mode)
  (setq which-key-idle-delay 1))

(add-hook 'before-save-hook 'delete-trailing-whitespace)

(global-visual-line-mode)

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

(require 'server)
(unless (server-running-p)
  (progn
    (server-start)
    (toggle-frame-maximized)
    )
)

(when (version<= "26.0.50" emacs-version )
  (global-display-line-numbers-mode))

(use-package mic-paren
    :config
    ;;(paren-activate)
    (add-hook 'c-mode-common-hook 'paren-activate)
    (add-hook 'python-mode-hook   'paren-activate)
    (add-hook 'org-mode-hook      'paren-activate)
)

;; move between windows with shift+[arrow]
    (windmove-default-keybindings)

(defun reload-init-file ()
  (interactive)
  (load-file user-init-file))

(global-set-key (kbd "C-c r") 'reload-init-file)

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

(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

(use-package try)

(setq inhibit-splash-screen t)
(scroll-bar-mode -1)
(menu-bar-mode -1)
(tool-bar-mode -1)

(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
(if (display-graphic-p)
    (load-theme 'neptune t))

(setq display-time-24hr-format t)
(display-time-mode 1)

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

(use-package spaceline
  :config
  (require 'spaceline-config)
  (setq powerline-default-separator (quote arrow))
(spaceline-spacemacs-theme))

(use-package diminish
  :init
  (diminish 'page-break-lines-mode)
  (diminish 'undo-tree-mode)
  (diminish 'org-src-mode)
  (diminish 'eldoc-mode))

(setq tramp-verbose 10)

(use-package magit
  :commands (magit-status magit-get-current-branch)
)

;; ivy gives intelligent file search with M-x
(use-package ivy
  :diminish
  :config
  (ivy-mode 1)
)

(use-package ivy-rich
:after ivy
:init
(ivy-rich-mode 1))

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

;; We'll try company-mode for now. The old standard autocomplete was the
  ;; smartly named auto-complete, but only company is being actively developed.
   (use-package company
   :hook
   ((emacs-lisp-mode . company-mode)
   (org-mode . company-mode)
   (c++-mode . company-mode)
   (c-mode . company-mode))
)

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

;;  (use-package projectile
;;    :diminish projectile-mode
;;    :config (projectile-mode)
;;    :bind-keymap
;;    ("C-c p" . projectile-command-map)
;;    :custom ((projectile-completion-system 'ivy))
;;    :init
;;    (when (file-directory-p "~/repos/")
;;      (setq projectile-project-search-path '("~/repos/")))
;;    )

;;  (use-package all-the-icons)
;;  ;; install if not present
;;  (unless (file-exists-p "~/.local/share/fonts/all-the-icons.ttf")
;;    (all-the-icons-install-fonts))

;; (use-package dashboard
;;   :config
;;   (dashboard-setup-startup-hook)
;;   (setq dashboard-startup-banner "~/.emacs.d/banner/banner.gif")
;;   (setq dashboard-items '((recents . 15)
;; 			   (projects . 5)
;; 			   (bookmarks . 5)
;; 			   (agenda . 5)
;; 			   (registers . 5)))
;;   ;; centering looks awful with multiple frames.
;;   (setq dashboard-center-content t)
;;   (setq dashboard-set-file-icons t)
;;   (setq dashboard-set-heading-icons t)
;;   (setq dashboard-footer-messages nil)
;;   (load-file "~/.emacs.d/dashboard_quotes.el")
;;   (setq dashboard-banner-logo-title (nth (random (length dashboard-quote-list)) dashboard-quote-list)))

;; Org-mode ------------------------------------------------------------
  (defun org-mode-setup ()
    (org-indent-mode)
    (dolist (face '((org-level-1 . 1.15)
                    (org-level-2 . 1.1)
                    (org-level-3 . 1.05)
                    (org-level-4 . 1.0)
                    (org-level-5 . 1.1)
                    (org-level-6 . 1.1)
                    (org-level-7 . 1.1)
                    (org-level-8 . 1.1)))
      (set-face-attribute (car face) nil :weight 'medium :height (cdr face)))
    )

  (use-package org
    :hook (org-mode . org-mode-setup)
    :commands (org-capture org-agenda)
    :config
    (setq org-ellipsis " ▾") ;; get rid of ugly orange underlining
    (require 'ox-md)   ;; Add markdown export support
    (message "ORG loaded")
    :bind
    ("C-c a" . org-agenda)
    )

  (use-package org-bullets
    :hook (org-mode . org-bullets-mode)
    :custom
    (org-bullets-bullet-list '("あ" "い" "う" "え" "お"))
    )

  ;; org agenda
  (setq org-agenda-files
        '("~/Dropbox/emacs/rac-agenda.org"
        "~/Dropbox/emacs/Birthdays.org"))
  (setq org-log-done 'time)


  ;; reveal.js presentations

  (use-package ox-reveal
    :after org-mode
    :config
    ;; We need to tell ox-reveal where to find the js file.
    ((setq org-reveal-root "http://cdn.jsdelivr.net/npm/reveal.js")
    (setq org-reveal-mathjax t))
)

(with-eval-after-load 'org
    (org-babel-do-load-languages
     'org-babel-load-languages
     '((emacs-lisp . t)
       (python . t))
     )
(setq org-confirm-babel-evaluate nil)
)

(defun rac/org-babel-tangle-config ()
  (when (string-equal (buffer-file-name)
                      (expand-file-name "~/repos/rac_dotfiles/.emacs.d/racinit.org"))
    (let ((org-confirm-babel-evaluate nil))
      (org-babel-tangle))))

  (add-hook 'org-mode-hook (lambda () (add-hook 'after-save-hook #'rac/org-babel-tangle-config)))

(defun rac/org-mode-visual-fill ()
  (setq visual-fill-column-width 95
        visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

(use-package visual-fill-column
  :defer t
  :hook (org-mode . rac/org-mode-visual-fill))
  :diminish

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

(use-package noflet)

(defun make-capture-frame ()
  "Create a new frame and run org-capture."
  (interactive)
  (make-frame '((name . "capture")))
  (select-frame-by-name "capture")
  (delete-other-windows)
  (noflet ((switch-to-buffer-other-window (buf) (switch-to-buffer buf)))
    (org-capture)))

(use-package flycheck
  :config
    (add-hook 'c-mode-hook 'flycheck-mode)
    (add-hook 'c-mode-hook '(lambda () (setq flycheck-gcc-language-standard "gnu99")))
    (add-hook 'c++-mode-hook 'flycheck-mode)
    ;;(add-hook 'python-mode-hook 'flycheck-mode)
    )

(use-package yasnippet
    :hook
    ((c-mode . yas-minor-mode)
    (c++-mode . yas-minor-mode)
    (python-mode . yas-minor-mode))
  )

  (use-package yasnippet-snippets
  :after yasnippet
)

(use-package blacken
	:hook (python-mode . blacken-mode)
;;    :config
;;	(add-hook 'python-mode-hook 'blacken-mode)
    )

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

(use-package tex
    :hook LaTeX-mode
    :ensure auctex
    :config
((setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq-default TeX-master nil)
(add-hook 'LaTeX-mode-hook 'visual-line-mode)
(add-hook 'LaTeX-mode-hook 'flyspell-mode)
(add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)
(setq reftex-plug-into-AUCTeX t))
)

(use-package auctex-latexmk
    :after tex
)

(use-package web-mode
  :hook (html-mode . web-mode)
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
  :after web-mode
  :config
  (add-hook 'sgml-mode-hook 'emmet-mode) ;; Auto-start on any markup modes
  (add-hook 'web-mode-hook 'emmet-mode) ;; Auto-start on any markup modes
  (add-hook 'css-mode-hook 'emmet-mode) ;; enable Emmet's css abbreviation.
)

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

(global-set-key (kbd "C-c b") 'org-publish-project)

(use-package htmlize
:defer 0
)

(use-package elfeed
    )
  (global-set-key (kbd "C-x w") 'elfeed)
  (setq-default elfeed-search-filter "@2-months-ago")
  (add-hook 'emacs-startup-hook (lambda () (run-at-time 0 120 'elfeed-update)))


(let ((elfeed-urls "~/Dropbox/emacs/rac_elfeeds.el"))
 (when (file-exists-p elfeed-urls)
   (load-file elfeed-urls))
)
