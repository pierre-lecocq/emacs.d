;;; my-looknfeel.el --- Emacs config - looknfeel

;; Time-stamp: <2015-12-06 22:18:10>
;; Copyright (C) 2015 Pierre Lecocq

;;; Commentary:

;;; Code:

(when (member "Inconsolata" (font-family-list))
  (set-face-attribute 'default nil
                      :family "Inconsolata" ;; "DejaVu Sans Mono"
                      :height 120
                      :weight 'normal
                      :width 'normal))

(when (display-graphic-p)
  (progn
    (toggle-frame-maximized)
    (setq show-paren-style 'expression
          select-enable-clipboard t))
  (set-fringe-mode 10))

(provide 'my-looknfeel)

;;; my-looknfeel.el ends here
