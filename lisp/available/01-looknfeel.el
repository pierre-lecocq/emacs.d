;;; 01-looknfeel.el --- Look'n'feel

;; Time-stamp: <2016-06-10 09:44:49>
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
                      :overline (color-lighten-name bg 15)
                      :underline (color-lighten-name bg 15)
                      :box nil)

  ;; mode-line-inactive
  (set-face-attribute 'mode-line-inactive nil
                      :foreground (color-lighten-name bg 20)
                      :background bg
                      :overline (color-lighten-name bg 15)
                      :underline (color-lighten-name bg 15)
                      :box nil)
  (custom-set-faces
   ;; string
   `(font-lock-string-face ((t (:foreground "IndianRed"))))

   ;; separator
   `(vertical-border ((t (:foreground ,(color-lighten-name bg 15)))))

   ;; which-func
   `(which-func ((t (:foreground "DodgerBlue"))))

   ;; search
   `(isearch ((t (:background "DodgerBlue" :foreground "white"))))

   ;; idle-highlight
   `(idle-highlight ((t (:inherit isearch))))

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

;; Transparency

(defun pl-transparency (value)
  "Set the transparency of the frame window.
Argument VALUE 0 is transparent, 100 is opaque."
  (interactive "nTransparency Value (0 - 100): ")
  (when (display-graphic-p)
    (set-frame-parameter (selected-frame) 'alpha value)))

;; Packages

(use-package idle-highlight-mode :ensure t)

(use-package rainbow-delimiters :ensure t)

;;; 01-looknfeel.el ends here