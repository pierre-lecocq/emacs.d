;;; 01-looknfeel.el --- Look'n'feel

;; Time-stamp: <2016-06-29 10:59:30>
;; Copyright (C) 2016 Pierre Lecocq

;;; Commentary:

;;; Code:

;; Colors

;; (use-package darkmine-theme :ensure t)
;; (use-package tao-theme :ensure t)

(when (display-graphic-p)
  (require 'color)
  (set-face-attribute 'fringe nil :background "grey13")
  (set-fringe-mode 10)
  (set-face-attribute 'mode-line nil :box nil)
  (set-face-attribute 'mode-line-inactive nil :box nil)
  (let ((bg (face-attribute 'default :background))
        (fg (face-attribute 'default :foreground)))
    (custom-set-faces
     `(font-lock-string-face ((t (:foreground "IndianRed"))))
     `(vertical-border ((t (:foreground ,(color-lighten-name bg 15)))))
     `(isearch ((t (:background "DodgerBlue" :foreground "white"))))
     `(which-func ((t (:inherit isearch))))
     `(idle-highlight ((t (:inherit isearch))))
     `(ido-subdir ((t (:inherit font-lock-function-name-face))))
     `(ido-first-match ((t (:inherit font-lock-variable-name-face))))
     `(ido-only-match ((t (:inherit font-lock-variable-name-face))))
     `(company-tooltip ((t (:inherit default :background ,(color-lighten-name bg 2)))))
     `(company-scrollbar-bg ((t (:background ,(color-lighten-name bg 10)))))
     `(company-scrollbar-fg ((t (:background ,(color-lighten-name bg 5)))))
     `(company-tooltip-selection ((t (:inherit font-lock-function-name-face))))
     `(company-tooltip-common ((t (:inherit font-lock-constant-face)))))))

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

(use-package autopair :ensure t
  :config (autopair-global-mode t))

(use-package anzu :ensure t
  :config (progn
            (global-anzu-mode +1)
            (set-face-attribute 'anzu-mode-line nil :foreground "yellow")))

(use-package idle-highlight-mode :ensure t)

(use-package rainbow-delimiters :ensure t)

;;; 01-looknfeel.el ends here
