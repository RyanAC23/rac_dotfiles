;;; neptune-theme.el --- neptune
;;; Version: 1.0
;;; Commentary:
;;; A theme called neptune
;;; Code:

(deftheme neptune "DOCSTRING for neptune")
  (custom-theme-set-faces 'neptune
   '(default ((t (:foreground "#6dc7ff" :background "#29222e" ))))
   '(cursor ((t (:background "#9d70b1" ))))
   '(fringe ((t (:background "#29222e" ))))
   '(mode-line ((t (:foreground "#1a012c" :background "#838383" ))))
   '(region ((t (:background "#423245" ))))
   '(secondary-selection ((t (:background "#4b3f50" ))))
   '(font-lock-builtin-face ((t (:foreground "#ab64b3" ))))
   '(font-lock-comment-face ((t (:foreground "#8d8d8d" ))))
   '(font-lock-function-name-face ((t (:foreground "#ffffff" ))))
   '(font-lock-keyword-face ((t (:foreground "#6fd3aa" ))))
   '(font-lock-string-face ((t (:foreground "#ffb3f5" ))))
   '(font-lock-type-face ((t (:foreground "#ac58ff" ))))
   '(font-lock-constant-face ((t (:foreground "#df37f1" ))))
   '(font-lock-variable-name-face ((t (:foreground "#5596c9" ))))
   '(minibuffer-prompt ((t (:foreground "#ffffff" :bold t ))))
   '(font-lock-warning-face ((t (:foreground "red" :bold t ))))
   )

;;;###autoload
(and load-file-name
    (boundp 'custom-theme-load-path)
    (add-to-list 'custom-theme-load-path
                 (file-name-as-directory
                  (file-name-directory load-file-name))))
;; Automatically add this theme to the load path

(provide-theme 'neptune)

;;; neptune-theme.el ends here
