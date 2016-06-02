;;; 01-looknfeel.el --- Look'n'feel

;; Time-stamp: <2016-06-02 20:21:59>
;; Copyright (C) 2016 Pierre Lecocq

;;; Commentary:

;;; Code:

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

(custom-set-faces '(which-func ((t (:foreground "selectedMenuItemColor")))))

(when (member "Inconsolata" (font-family-list))
  (set-face-attribute 'default nil
                      :family "Inconsolata" ;; "DejaVu Sans Mono"
                      :height 120
                      :weight 'normal
                      :width 'normal))

(use-package idle-highlight-mode :ensure t)

;;; 01-looknfeel.el ends here
