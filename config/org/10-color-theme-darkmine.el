(defun darkmine ()
  (interactive)
  (color-theme-install
   '(darkmine
     ((background-color . "#212121")
      (foreground-color . "#e3e3e3")
      (background-mode . dark)
      (border-color . "#232323")
      (cursor-color . "#08CA5F")
      (mouse-color . "#323232"))

     (mode-line ((t (:foreground "#FFFFFF" :background "#323232"))))
     (region ((t (:background "#353535"))))

     (font-lock-comment-face ((t (:foreground "#a5a5a5"))))
     (font-lock-constant-face ((t (:foreground "#8dd7e9"))))
     (font-lock-builtin-face ((t (:foreground "#08CA5F"))))
     (font-lock-function-name-face ((t (:foreground "#c8f1fa"))))
     (font-lock-variable-name-face ((t (:foreground "#d46a6a"))))
     (font-lock-keyword-face ((t (:foreground "#5FB7CC"))))
     (font-lock-string-face ((t (:foreground "#aa3939"))))
     (font-lock-doc-string-face ((t (:foreground "#d46a6a"))))
     (font-lock-type-face ((t (:foreground "#08CA5F"))))

     )))

(provide 'darkmine)
