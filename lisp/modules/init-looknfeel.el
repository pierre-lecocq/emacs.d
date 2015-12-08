;;; init-looknfeel.el --- Emacs config - looknfeel

;; Time-stamp: <2015-12-09 00:32:33>
;; Copyright (C) 2015 Pierre Lecocq

;;; Commentary:

;;; Code:

(require 'linum)
(require 'paren)
(require 'time-stamp)

(auto-compression-mode 1)
(column-number-mode 1)
(global-auto-revert-mode 1)
(global-font-lock-mode 1)
(global-hl-line-mode 1)
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

(setq-default mode-line-format
              (list
               '(:eval (if (buffer-modified-p)
                           (propertize " %b" 'face 'bold-italic)
                         (propertize " %b" 'face 'bold)))
               " | %l:%c %p:%I | %m";; (format " %s" minor-mode-alist)
               '(which-function-mode (" " which-func-format))
               '(vc-mode vc-mode)))

;;; init-mode-line.el ends here

(use-package autopair :ensure t
  :init (autopair-global-mode t))

(use-package anzu :ensure t
  :init (progn
          (global-anzu-mode +1)
          (set-face-attribute 'anzu-mode-line nil :foreground "yellow")))

(use-package buffer-move :ensure t
  :bind (("<C-S-up>"    . buf-move-up)
         ("<C-S-down>"  . buf-move-down)
         ("<C-S-left>"  . buf-move-left)
         ("<C-S-right>" . buf-move-right)))

(use-package bonjourmadame :ensure t)

(use-package browse-kill-ring :ensure t)

(use-package darkmine-theme :ensure t
  :init (load-theme 'darkmine t))

(use-package idle-highlight-mode :ensure t)

(use-package symon :ensure t
  :init (progn
          (setq symon-delay 5)
          (symon-mode t)))

(provide 'init-looknfeel)

;;; init-looknfeel.el ends here
