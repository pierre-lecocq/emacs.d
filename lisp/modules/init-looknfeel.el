;;; init-looknfeel.el --- Emacs config - looknfeel

;; Time-stamp: <2015-12-07 13:09:49>
;; Copyright (C) 2015 Pierre Lecocq

;;; Commentary:

;;; Code:

(setq frame-title-format "Emacs %f"
      time-stamp-format "%:y-%02m-%02d %02H:%02M:%02S")

(setq-default show-trailing-whitespace t
              highlight-tabs t
              mode-line-format
              (list
               '(:eval (if (buffer-modified-p)
                           (propertize "  %b" 'face 'bold-italic)
                         (propertize "  %b" 'face 'bold)))
               " - %l:%c %p:%I - %m";; (format " %s" minor-mode-alist)
               '(which-function-mode (" " which-func-format))))

(when (display-graphic-p)
  (progn
    (toggle-frame-maximized)
    (setq show-paren-style 'expression
          select-enable-clipboard t))
  (set-fringe-mode 10))

(when (member "Inconsolata" (font-family-list))
  (set-face-attribute 'default nil
                      :family "Inconsolata" ;; "DejaVu Sans Mono"
                      :height 120
                      :weight 'normal
                      :width 'normal))

(provide 'init-looknfeel)

;;; init-looknfeel.el ends here
