;;; init-looknfeel.el --- Emacs config - looknfeel

;; Time-stamp: <2015-12-07 00:03:13>
;; Copyright (C) 2015 Pierre Lecocq

;;; Commentary:

;;; Code:

(setq frame-title-format "Emacs %f"
      time-stamp-format "%:y-%02m-%02d %02H:%02M:%02S")

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

(provide 'init-looknfeel)

;;; init-looknfeel.el ends here
