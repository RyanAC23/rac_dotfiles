;;; neptune-light-theme.el --- neptune-light
;;; Version: 1.0
;;; Commentary:
;;; A theme called neptune-light
;;; Code:

(deftheme neptune-light "DOCSTRING for neptune-light")
  (custom-theme-set-faces 'neptune-light
   '(default ((t (:foreground "#7b3fa7" :background "#ffffff" ))))
   '(cursor ((t (:background "#9d70b1" ))))
   '(fringe ((t (:background "#f2e1ff" ))))
   '(mode-line ((t (:foreground "#000000" :background "#ffffff" ))))
   '(region ((t (:background "#f3b2ff" ))))
   '(secondary-selection ((t (:background "#bf00e2" ))))
   '(font-lock-builtin-face ((t (:foreground "#9115da" ))))
   '(font-lock-comment-face ((t (:foreground "#d000da" ))))
   '(font-lock-function-name-face ((t (:foreground "#9100a7" ))))
   '(font-lock-keyword-face ((t (:foreground "#000000" ))))
   '(font-lock-string-face ((t (:foreground "#008f9f" ))))
   '(font-lock-type-face ((t (:foreground "#0563df" ))))
   '(font-lock-constant-face ((t (:foreground "#e700ff" ))))
   '(font-lock-variable-name-face ((t (:foreground "#725487" ))))
   '(minibuffer-prompt ((t (:foreground "#000000" :bold t ))))
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

;;; neptune-light-theme.el ends here
