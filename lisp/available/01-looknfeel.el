;;; 01-looknfeel.el --- Look'n'feel

;; Time-stamp: <2016-06-03 09:08:24>
;; Copyright (C) 2016 Pierre Lecocq

;;; Commentary:

;;; Code:

;; Colors

(require 'color)

(set-face-attribute 'mode-line nil
                    :foreground "#d5d5d5"
                    :background "#333333"
                    :overline "#666666"
                    :underline "#666666"
                    :box nil)

(set-face-attribute 'mode-line-inactive nil
                    :foreground "#a5a5a5"
                    :background "#222222"
                    :overline "#444444"
                    :underline "#444444"
                    :box nil)

(let ((bg (face-attribute 'default :background)))
  (custom-set-faces
   `(which-func ((t (:foreground "cornflower blue"))))
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
