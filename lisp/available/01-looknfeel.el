;;; 01-looknfeel.el --- Look'n'feel

;; Time-stamp: <2016-06-03 13:34:29>
;; Copyright (C) 2016 Pierre Lecocq

;;; Commentary:

;;; Code:

;; Colors

(require 'color)

(let ((bg (face-attribute 'default :background))
      (fg (face-attribute 'default :foreground)))
  ;; mode-line
  (set-face-attribute 'mode-line nil
                      :foreground (color-darken-name fg 20)
                      :background (color-lighten-name bg 5)
                      :overline (color-lighten-name bg 20)
                      :underline (color-lighten-name bg 20)
                      :box nil)
  ;; mode-line-inactive
  (set-face-attribute 'mode-line-inactive nil
                      :foreground (color-lighten-name bg 20)
                      :background bg
                      :overline (color-lighten-name bg 10)
                      :underline (color-lighten-name bg 10)
                      :box nil)
  (custom-set-faces
   ;; which-func
   `(which-func ((t (:foreground "cornflower blue"))))
   ;; company
   `(company-tooltip ((t (:inherit default :background ,(color-lighten-name bg 2)))))
   `(company-scrollbar-bg ((t (:background ,(color-lighten-name bg 10)))))
   `(company-scrollbar-fg ((t (:background ,(color-lighten-name bg 5)))))
   `(company-tooltip-selection ((t (:inherit font-lock-function-name-face))))
   `(company-tooltip-common ((t (:inherit font-lock-constant-face))))))

;; Fonts

(when (member "Inconsolata" (font-family-list))
  (set-face-attribute 'default nil
                      :family "Inconsolata" ;; "DejaVu Sans Mono"
                      :height 120
                      :weight 'normal
                      :width 'normal))

;; Packages

(use-package idle-highlight-mode :ensure t)

;;; 01-looknfeel.el ends here
