;;; neptune-light.el --- neptune
;;; Version: 0.1
;;; Commentary:
;;; A lighter Neptune theme.
;;; Code:

(deftheme neptune-light "DOCSTRING for neptune")
  (custom-theme-set-faces 'neptune-light
   '(default ((t (:foreground "#ffffff" :background "#5a2c7c" ))))
   '(cursor ((t (:background "#9d70b1" ))))
   '(fringe ((t (:background "#29222e" ))))
   '(mode-line ((t (:foreground "#1a012c" :background "#838383" ))))
   '(region ((t (:background "#423245" ))))
   '(secondary-selection ((t (:background "#4b3f50" ))))
   '(font-lock-builtin-face ((t (:foreground "#c767ff" ))))
   '(font-lock-comment-face ((t (:foreground "#e9afff" ))))
   '(font-lock-function-name-face ((t (:foreground "#ec71ff" ))))
   '(font-lock-keyword-face ((t (:foreground "#8dbeff" ))))
   '(font-lock-string-face ((t (:foreground "#6fd3aa" ))))
   '(font-lock-type-face ((t (:foreground "#75c3ff" ))))
   '(font-lock-constant-face ((t (:foreground "#f595ff" ))))
   '(font-lock-variable-name-face ((t (:foreground "#93deff" ))))
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

(provide-theme 'neptune-light)

;;; neptune-light.el ends here
