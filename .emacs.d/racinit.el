(defvar rac/default-bigger-font-size 135)
(defvar rac/fixed-pitch-bigger-font-size 130)
(defvar rac/default-font-size 120)
(defvar rac/fixed-pitch-font-size 120)
(defvar rac/default-variable-font-size 120)

      ;; (set-face-attribute 'default nil :font "Palatino" :height
      ;;    		  rac/default-bigger-font-size)
      ;; (set-face-attribute 'variable-pitch nil :font "Cantarell" :height
      ;; 		    rac/default-variable-font-size :weight 'regular)
      ;; (set-face-attribute 'fixed-pitch nil :font "Palatino" :height
      ;; 		    rac/fixed-pitch-bigger-font-size)

      (set-face-attribute 'default nil :font "Fira Code Retina" :height
			  rac/default-font-size)
      (set-face-attribute 'fixed-pitch nil :font "Fira Code Retina" :height
			    rac/default-font-size)
      (set-face-attribute 'variable-pitch nil :font "Cantarell" :height
			    rac/default-variable-font-size :weight 'regular)

(defvar rac/bytes-per-KiB 1024
    "Number of bytes in a kibibyte (KiB).")

  (defvar rac/bytes-per-MiB (* rac/bytes-per-KiB 1024)
    "Number of bytes in a mebibyte (MiB).")

    (setq gc-cons-threshold (* 100 rac/bytes-per-MiB))
    ;"GC threshold during startup: 100 MiB."

    (defun rac/run-startup-diagnostics ()
      "A short profile of emacs startup."
      (add-hook 'emacs-startup-hook
                (lambda ()
                  (message "*** Emacs loaded in %s with %d garbage collections."
                           (format "%.2f seconds"
                                   (float-time
                                    (time-subtract after-init-time before-init-time)))
                           gcs-done))))
(setq use-package-verbose t)
(rac/run-startup-diagnostics)

(require 'cl-lib)

(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)

(fset 'yes-or-no-p 'y-or-n-p)

(defun reload-init-file ()
  (interactive)
  (load-file user-init-file))

(global-set-key (kbd "C-c r") 'reload-init-file)

(windmove-default-keybindings)

(when (version<= "26.0.50" emacs-version )
  (global-display-line-numbers-mode))

(add-hook 'before-save-hook 'delete-trailing-whitespace)

(global-visual-line-mode)

(require 'bind-key)
(bind-key* "C-n" 'next-logical-line)

(defvar --backup-directory (concat user-emacs-directory "backups"))
(if (not (file-exists-p --backup-directory))
    (make-directory --backup-directory t))
(setq backup-directory-alist `(("." . ,--backup-directory)))
(setq make-backup-files t          ; backup of a file the first time it is saved.
      backup-by-copying t          ; don't clobber symlinks
      version-control t            ; version numbers for backup files
      delete-old-versions t        ; delete excess backup files silently
      delete-by-moving-to-trash t
      kept-old-versions 2          ; oldest versions to keep when a new numbered backup is made (default: 2)
      kept-new-versions 2          ; newest versions to keep when a new numbered backup is made (default: 2)
      auto-save-default nil          ; auto-save every buffer that visits a file
      auto-save-timeout 20         ; number of seconds idle time before auto-save (default: 30)
      auto-save-interval 200)       ; number of keystrokes between auto-saves (default: 300)

(use-package server
:ensure nil
:config
(unless (server-running-p)
    (server-start)
    (toggle-frame-maximized)))

(defvar current-date-format "%A %d %B %Y"
  "Format of date to insert with `insert-current-date' func
  See help of `format-time-string' for possible replacements")

(defvar current-date-time-format "%a %d %B %Y %H:%M:%S %Z"
  "Format of date to insert with `insert-current-date-time' func
  See help of `format-time-string' for possible replacements")

(defvar current-time-format "%a %H:%M:%S"
  "Format of date to insert with `insert-current-time' func.
  Note the weekly scope of the command's precision.")

(defun insert-current-date ()
  "insert the current date and time into current buffer.
Uses `current-date-time-format' for the formatting the date/time."
  (interactive)
  (cond
   ((equal major-mode 'markdown-mode)
    (insert "# "))
   ((equal major-mode 'org-mode)
    (insert "* "))
   (t
    (insert "# ---------\n# ")))
  (insert (format-time-string current-date-format))
  (insert "\n"))

(defun insert-current-date-time ()
  "insert the current date and time into current buffer.
  Uses `current-date-time-format' for the formatting the date/time."
  (interactive)
  (insert "========================================\n")
  (insert (format-time-string current-date-time-format (current-time)))
  (insert "\n"))

(defun insert-current-time ()
  "insert the current time (1-week scope) into the current buffer."
  (interactive)
  (insert "- ")
  (insert (format-time-string current-time-format (current-time)))
  (insert " "))

(global-set-key "\C-x\C-d" 'insert-current-date)
(global-set-key "\C-x\C-t" 'insert-current-time)

;(require 'use-package)
(require 'use-package-ensure)
(setq use-package-always-ensure t)

(use-package try
  :defer t)

(use-package which-key
  :defer 0
  :diminish which-key-mode
  :config
  (which-key-mode)
  (setq which-key-idle-delay 1))

(use-package mic-paren
  :config
  (paren-activate))

;;; Place to put local packages.
(let* ((path (expand-file-name "lisp" user-emacs-directory))
       (local-pkgs (mapcar 'file-name-directory (directory-files-recursively path ".*\\.el"))))
  (if (file-accessible-directory-p path)
      (mapc (apply-partially 'add-to-list 'load-path) local-pkgs)
    (make-directory path :parents)))

(setq inhibit-splash-screen t)
(scroll-bar-mode -1)
(menu-bar-mode -1)
(tool-bar-mode -1)

(use-package rainbow-mode
  :ensure t)

(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
(if (display-graphic-p)
    ;;(load-theme 'neptune t))
    (load-theme 'xemacs t))

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
  (spaceline-spacemacs-theme)
  (spaceline-toggle-projectile-root-off))

(use-package diminish
  :after spaceline
  :init
  (dolist (diminish-list '(page-break-lines-mode
			   undo-tree-mode
			   org-src-mode
			   eldoc-mode
			   visual-line-mode
			   org-indent-mode
			   ))
    (diminish diminish-list)))

;; ivy gives intelligent file search with M-x
(use-package ivy
  :diminish
  :config
  (ivy-mode 1))

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
         ("M-p" . (lambda () (interactive) (search-backward (car swiper-history)))))
  :config
  (progn
    (setq ivy-use-virtual-buffers t)
    (setq ivy-display-style 'fancy)
    (define-key read-expression-map (kbd "C-r") 'counsel-expression-history)))

(use-package ibuffer
  :ensure nil; ; ibuffer is built-in, so don't try to install it from melpa.
  :bind ("C-x C-b" . ibuffer)
  :hook (ibuffer-mode . (lambda ()
            (ibuffer-switch-to-saved-filter-groups "default")))
  :config
  ;; Don't show filter groups if there are no filters in the group
  (setq ibuffer-show-empty-filter-groups nil)
  ;; Don't ask for confirmation to delete unmodified buffers
  (setq ibuffer-expert t)
  ;; categorize buffers by project/language groups:
  (setq ibuffer-saved-filter-groups
        (quote (("default"
                 ("python" (mode . python-mode))
                 ("c/c++" (or
                           (mode . c-mode)
                           (mode . c++-mode)))
                 ("org" (mode . org-mode))
                 ("TeX" (or (filename . ".tex")
                            (filename . ".sty")))
                 ("docs" (mode . markdown-mode))
                 ("web" (or
                         (mode . mhtml-mode)
                         (mode . html-mode)
                         (mode . css-mode)))
                 ("emacs" (or
                           (name . "^\\*scratch\\*$")
                           (name . "^\\*Warnings\\*$")
                           (name . "^\\*Messages\\*$")))
                 ("Dired" (mode . dired-mode))
                 ))))

  (defun rac/--human-readable-file-sizes-to-bytes (string)
    "Convert a human-readable file size into bytes."
    (cond
     ((string-suffix-p "G" string t)
      (* 1000000000 (string-to-number (substring string 0 (- (length string) 1)))))
     ((string-suffix-p "M" string t)
      (* 1000000 (string-to-number (substring string 0 (- (length string) 1)))))
     ((string-suffix-p "K" string t)
      (* 1000 (string-to-number (substring string 0 (- (length string) 1)))))
     (t
      (string-to-number (substring string 0 (- (length string) 1))))))

  (defun rac/--bytes-to-human-readable-file-sizes (bytes)
    "Convert number of bytes to human-readable file size."
    (cond
     ((> bytes 1000000000) (format "%10.1fG" (/ bytes 1000000000.0)))
     ((> bytes 100000000) (format "%10.0fM" (/ bytes 1000000.0)))
     ((> bytes 1000000) (format "%10.1fM" (/ bytes 1000000.0)))
     ((> bytes 100000) (format "%10.0fk" (/ bytes 1000.0)))
     ((> bytes 1000) (format "%10.1fk" (/ bytes 1000.0)))
     (t (format "%10d" bytes))))

  ;; Use human readable Size column instead of original one
  (define-ibuffer-column size-h
    (:name "Size"
           :summarizer
           (lambda (column-strings)
             (let ((total 0))
               (dolist (string column-strings)
                 (setq total
                       (+ (float (rac/--human-readable-file-sizes-to-bytes string))
                          total)))
               (rac/--bytes-to-human-readable-file-sizes total)))); :summarizer nil
    (rac/--bytes-to-human-readable-file-sizes (buffer-size)))

  ;; Modify the default ibuffer-formats
  (setq ibuffer-formats
        '((mark modified read-only locked " "
                (name 30 30 :left :elide)
                " "
                (size-h 11 -1 :right)
                " "
                (mode 16 16 :left :elide)
                " "
                filename-and-process)
          (mark " "
                (name 33 33)
                " " filename))))

(use-package projectile
    :diminish projectile-mode
    :config (projectile-mode)
    :bind-keymap
    ("C-c p" . projectile-command-map)
    :custom ((projectile-completion-system 'ivy))
    :init
    (when (file-directory-p "~/repos/")
      (setq projectile-project-search-path '("~/repos/"))))

  (use-package all-the-icons)

  ;; install if not present
  (unless (file-exists-p "~/.local/share/fonts/all-the-icons.ttf")
    (all-the-icons-install-fonts))

(defun rac/load-if-exists (file)
  "Load file if it exists."
  ( when (file-exists-p file)
    (load-file file)))

  (use-package dashboard
    :config
    (dashboard-setup-startup-hook)
    (setq dashboard-startup-banner "~/.emacs.d/banner/Aoba.png")
    (setq dashboard-items '((projects . 10)
                            (recents . 15)
                            (bookmarks . 5)
                            (registers . 5)))
    (setq dashboard-center-content t)
    (setq dashboard-set-file-icons t)
    (setq dashboard-set-heading-icons t)
    (setq dashboard-footer-messages nil)
    (rac/load-if-exists "~/.emacs.d/dashboard_quotes.el")
    (setq dashboard-banner-logo-title (nth (random (length dashboard-quote-list)) dashboard-quote-list))
    )

;; Org-mode ------------------------------------------------------------
(defun org-mode-setup ()
  (org-indent-mode)
  (dolist (face '((org-level-1 . 1.3)
                  (org-level-2 . 1.2)
                  (org-level-3 . 1.1)
                  (org-level-4 . 1.0)
                  (org-level-5 . 1.1)
                  (org-level-6 . 1.1)
                  (org-level-7 . 1.1)
                  (org-level-8 . 1.1)))
    (set-face-attribute (car face) nil :font "Cantarell" :weight 'regular :height (cdr face)))
  (set-face-attribute 'org-block nil :foreground nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-code nil   :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-table nil   :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch))

;;(org-mode-setup)
(defun org-winmove-setup()
  (setq-local windmove-mode nil)
  (add-hook 'org-shiftup-final-hook 'windmove-up)
  (add-hook 'org-shiftleft-final-hook 'windmove-left)
  (add-hook 'org-shiftdown-final-hook 'windmove-down)
  (add-hook 'org-shiftright-final-hook 'windmove-right))

(defun org-note-insert-page ()
  "Prompt user to enter a number, with input history support."
  (interactive)
  (let (n)
    (setq n (read-number "Enter a page number: " ))
    (end-of-line)
    (insert "\n- ")
    (insert (format "(%d) " n))))

(use-package org
  :hook
  ((org-mode . org-mode-setup)
   (org-mode . org-winmove-setup))
  :config
  (setq org-ellipsis " ▾") ;; get rid of ugly orange underlining
  (require 'ox-md)   ;; Add markdown export support
  :bind
  ("C-p"   . org-note-insert-page))

(use-package org-bullets
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("あ" "い" "う" "え" "お")))
(setq org-log-done 'time)

;; reveal.js presentations
(use-package ox-reveal
  :config
  ;; We need to tell ox-reveal where to find the js file.
  (dolist (setq '(
                  (org-reveal-root "http://cdn.jsdelivr.net/npm/reveal.js")
                  (org-reveal-mathjax t)))))

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

(with-eval-after-load 'org
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (python . t)))
  (setq org-confirm-babel-evaluate nil))

(setq link-capture-string "| [[%^{LINK}][%^{TITLE}]] | %^{NOTES} | %^g | %t |")
(setq quote-capture-string "# %T\n#+BEGIN_QUOTE\n/%^{QUOTE}/\n\t--%^{SOURCE}\n#+END_QUOTE\n\n%?")

(setq org-capture-templates
      `(
        ("1" "Links : Geofront" table-line (file+headline
                                            "~/Dropbox/website/org/capture/links-general.org" "Links")
         ,link-capture-string :kill-buffer t)
        ("2" "Links : NERV Headquarters" table-line (file+headline
                                                     "~/Dropbox/website/org/capture/links-focused.org" "Other")
         ,link-capture-string :kill-buffer t)
        ("3" "Links : Central Dogma" table-line (file+headline                                       "~/Dropbox/website/org/capture/links-private.org" "Links")
         ,link-capture-string :kill-buffer t)
        ("n" "Links : Nabokov" table-line (file+headline
                                           "~/Dropbox/website/org/capture/links-general.org" "Nabokovia")
         ,link-capture-string :kill-buffer t)
        ("q" "new quote" plain (file+headline "~/Dropbox/website/org/geocite/other/other-content-index.org" "Quotes")
         :prepend t :kill-buffer t)
        ("g" "Links : Games [Geofront]" table-line (file+headline
                                                    "~/Dropbox/website/org/capture/links-general.org" "Game")
         ,link-capture-string :kill-buffer t)
        ))

(defun rac/org-babel-tangle-config ()
  (when (string-equal (buffer-file-name)
                      (expand-file-name "~/repos/rac_dotfiles/.emacs.d/racinit.org"))
    (let ((org-confirm-babel-evaluate nil))
      (org-babel-tangle))))

(add-hook 'after-save-hook #'rac/org-babel-tangle-config)

(defun rac/org-mode-visual-fill ()
  (setq visual-fill-column-width 150
        visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

(use-package visual-fill-column
  :defer t
  :hook (org-mode . rac/org-mode-visual-fill)
  :diminish)

(use-package org-roam
  :ensure t
  :init
  (setq org-roam-v2-ack t)
  :custom
  (org-roam-directory "~/Dropbox/emacs/Roam/db")
  (org-roam-completion-everywhere t)
  (org-roam-capture-templates
   '(("n" "note: default" plain
      "%?"
      :if-new (file+head "%<%Y%m%d>-${slug}.org" "#+title: ${title}\n")
      :unnarrowed t)
     ("a" "author" plain
      "* Bio\n\n- year: %?\n- Birthplace: %?\n- Other: %?\n\n"
      :if-new (file+head "%<%Y%m%d>-${slug}.org" "#+title: ${title}\n")
      :unnarrowed t)
     ("b" "book" plain
      (file "~/Dropbox/emacs/Roam/templates/book_template.org")
      :if-new (file+head "%<%Y%m%d>-${slug}.org" "#+title: ${title}\n")
      :unnarrowed t)))
  :bind (("C-c n l" . org-roam-buffer-toggle)
       ("C-c n f" . org-roam-node-find)
       ("C-c n i" . org-roam-node-insert)
       ("C-c n I" . org-roam-node-insert-immediate)
       ("C-c n c" . org-id-get-create)
       :map org-mode-map
       ("C-M-i" . completion-at-point))
  :config
  (org-roam-setup)
  				      ; The following snippet allows searching for tags using `org-roam-node-find`.
  				      ;  [[https://github.com/org-roam/org-roam/pull/2054]]
  (setq org-roam-node-display-template
      (concat "${title:*} "
  	      (propertize "${tags:10}" 'face 'org-tag)))
  )

(use-package org-roam-ui
  :ensure t)

(use-package bibtex
  :ensure async)
(setq bibtex-autokey-year-length 4
      bibtex-autokey-name-year-separator "-"
      bibtex-autokey-year-title-separator "-"
      bibtex-autokey-titleword-separator "-"
      bibtex-autokey-titlewords 2
      bibtex-autokey-titlewords-stretch 1
      bibtex-autokey-titleword-length 5)

;; (with-eval-after-load "org"
;; (add-to-list 'org-src-lang-modes '("latex-macros" . latex)))

(defvar org-babel-default-header-args:latex-macros
  '((:results . "raw")
    (:exports . "results")))

(defun prefix-all-lines (pre body)
  (with-temp-buffer
    (insert body)
    (string-insert-rectangle (point-min) (point-max) pre)
    (buffer-string)))

(defun org-babel-execute:latex-macros (body _params)
  (concat
   (prefix-all-lines "#+LATEX_HEADER: " body)
   "\n#+HTML_HEAD_EXTRA: <div style=\"display: none\"> \\(\n"
   (prefix-all-lines "#+HTML_HEAD_EXTRA: " body)
   "\n#+HTML_HEAD_EXTRA: \\)</div>\n"))

(use-package auctex
  :mode (("\\.tex\\'" . latex-mode)
         ("\\.sty\\'" . latex-mode))
  :bind ("C-<return>" . compile)
  :config
  (setq TeX-electric-sub-and-superscript t)
  )

(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

  (use-package flycheck
    :ensure t
    :config
    (add-hook 'after-init-hook #'global-flycheck-mode)
    ;; Set the gcc language standard.
    (add-hook 'c++-mode-hook '(lambda () (setq flycheck-gcc-language-standard "c++23")))
    ;; Tell cppcheck to use c++23.
    (setq flycheck-cppcheck-standards '("c++23"))
    (add-hook 'c++-mode-hook
            '(lambda ()
               (setq flycheck-gcc-args '("-std=c++23"))))
    )

(use-package company
  :hook
  ((emacs-lisp-mode
    c++-mode
    c-mode
    lsp-mode
    python-mode)
   . company-mode)
  :bind (:map company-active-map
              ("<tab>" . company-complete-selection))
  :custom
  (company-minimum-prefix-length 1)
  (company-idle-delay 0.0)
  :custom-face
  (company-tooltip
   ((t (:family "Terminus")))))

(use-package blacken
  :hook (python-mode . blacken-mode))

(use-package format-all
  :ensure t
  :hook ((c-mode . format-all-mode)
         (c++-mode . format-all-mode))
  :config
  (setq-default format-all-formatters '(("C++" clang-format))))

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
                                                 nil))))
(add-hook 'python-mode-hook 'python-remap-fs)

(use-package python
  :ensure t
  :custom
  (python-shell-interpreter "python3"))

(use-package conda
  :after python
  :config
  (custom-set-variables
   '(conda-anaconda-home "~/.conda/"))
  (setq conda-env-home-directory (expand-file-name "~/miniconda3/"))
  (conda-env-activate "~/miniconda3/"))

(use-package markdown-mode
  :ensure t
  :mode ("README\\.md\\'" . gfm-mode)
  :init (setq markdown-command "multimarkdown")
  )

(use-package yasnippet
  :ensure t
  :config
  (add-to-list 'yas-snippet-dirs (locate-user-emacs-file "snippets"))
  (yas-reload-all)
  (yas-global-mode 1)
  :hook
  (markdown-mode . (lambda ()
                     (yas-activate-extra-mode 'latex-mode)))
  :bind
  ("C-<tab>" . yas-expand)
  )

(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :init (setq lsp-keymap-prefix "C-c l")
  :config
  (setq lsp-clients-clangd-executable "/usr/bin/clangd"
        lsp-headerline-breadcrumb-enable nil
        lsp-enable-which-key-integration t
        lsp-signature-auto-activate nil
        lsp-diagnostics-provider :flycheck
  ))

(use-package lsp-ui
  :commands lsp-ui-mode
  :after lsp-mode
  :config
  (setq lsp-ui-doc-enable t
        lsp-ui-doc-position 'at-point))

  (add-hook 'c-mode-common-hook
            (lambda ()
              (local-set-key (kbd "C-<return>") 'compile)))

(setq tramp-verbose 3)

(use-package magit
  :commands (magit-status magit-get-current-branch)
  :bind ("C-c g" . magit-status))

(rac/load-if-exists "~/.emacs.d/websites.el")

(global-set-key (kbd "C-c b") 'org-publish-project)

(use-package htmlize
  :defer 0)

(setq gc-cons-threshold (* 50 rac/bytes-per-MiB))
  ;"GC threshold during runtime: 50 MiB."

(desktop-save-mode t)

(setq your-own-path default-directory)
(if (file-exists-p
     (concat your-own-path ".emacs.desktop"))
    (desktop-read your-own-path))

(add-hook 'kill-emacs-hook
      `(lambda ()
        (desktop-save ,your-own-path t)))
