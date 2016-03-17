;;; pl-looknfeel.el --- Emacs config - looknfeel

;; Time-stamp: <2016-03-17 21:41:48>
;; Copyright (C) 2015 Pierre Lecocq

;;; Commentary:

;;; Code:

(defvar pl-current-theme-name)

(require 'linum)
(require 'paren)
(require 'time-stamp)

(auto-compression-mode 1)
(column-number-mode 1)
(global-auto-revert-mode 1)
(global-font-lock-mode 1)
;; (global-hl-line-mode 1)
(line-number-mode 1)
(show-paren-mode 1)
(transient-mark-mode 1)
(which-function-mode 1)

(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)

(when (display-graphic-p)
  (set-fringe-mode 10)
  (toggle-frame-maximized)
  (setq show-paren-style 'expression
        select-enable-clipboard t))

(when (member "Inconsolata" (font-family-list))
  (set-face-attribute 'default nil
                      :family "Inconsolata" ;; "DejaVu Sans Mono"
                      :height 120
                      :weight 'normal
                      :width 'normal))

(use-package autopair :ensure t
  :init (autopair-global-mode t))

(use-package anzu :ensure t
  :init (progn
          (global-anzu-mode +1)
          (set-face-attribute 'anzu-mode-line nil :foreground "yellow")))

(use-package bonjourmadame :ensure t)

(use-package browse-kill-ring :ensure t)

(use-package idle-highlight-mode :ensure t)

(use-package symon :ensure t
  :init (progn
          (setq symon-delay 5)
          (symon-mode t)))

(provide 'pl-looknfeel)

;;; pl-looknfeel.el ends here
