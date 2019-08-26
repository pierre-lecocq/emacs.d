;;; init-theme.el --- Theme init -*- lexical-binding: t; -*-

;; Time-stamp: <2019-08-26 16:08:51>
;; Copyright (C) 2019 Pierre Lecocq

;;; Commentary:

;;; Code:

(if (eq system-type 'darwin)
    (toggle-frame-fullscreen)
  (toggle-frame-maximized))

(when (and window-system
           (eq system-type 'darwin)
           (not (version< emacs-version "26.1")))
  (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
  (add-to-list 'default-frame-alist '(ns-appearance . dark)))

;; Colors

(set-background-color "#1a1a1a")
(set-foreground-color "#fafafa")
(set-face-background 'region "DodgerBlue")
(set-face-foreground 'font-lock-comment-face "#6a6a6a")
(set-face-foreground 'font-lock-string-face "#ff6666")
(set-face-background hl-line-face "#2a2a2a")
(set-face-background 'vertical-border "#4a4a4a")
(set-face-foreground 'vertical-border (face-background 'vertical-border))

;; Fringe

(setq-default left-fringe-width 20
              right-fringe-width 20)

(set-face-attribute 'fringe nil
                    :foreground (face-foreground 'default)
                    :background (face-background 'default))

;; Modeline

(set-face-background 'mode-line "#2a2a2a")
(set-face-foreground 'mode-line "#eaeaea")
(set-face-background 'mode-line-inactive "#2a2a2a")
(set-face-foreground 'mode-line-inactive "#8a8a8a")
(set-face-attribute 'mode-line-inactive nil :box '(:style pressed-button))

(use-package minions :ensure t
  :init (setq minions-mode-line-lighter "..."
              minions-direct '(flyspell-mode projectile-mode))
  :config (minions-mode 1))

;; Icons

(use-package all-the-icons :ensure t) ;; Run `M-x all-the-icons-install-fonts'

(use-package all-the-icons-dired :ensure t
  :custom-face (all-the-icons-dired-dir-face ((t (:foreground nil))))
  :hook (dired-mode . all-the-icons-dired-mode))

;; Visual help

(defmacro toggle-that-mode (mode)
  "Toggle MODE."
  `(,mode (if (bound-and-true-p ,mode) -1 1)))

(defun toggle-show-paren-mode-style ()
  "Toggle `show-paren-mode' style."
  (interactive)
  (let* ((min 'parenthesis)
         (max 'expression)
         (style (if (equal show-paren-style min) max min)))
    (setq show-paren-style style)))

(global-set-key (kbd "C-c v p") 'toggle-show-paren-mode-style)

(use-package fill-column-indicator :ensure t
  :init (setq fci-rule-column 80
              fci-rule-color "#2a2a2a"))

(defun toggle-fill-column-indicator ()
  "Toggle `fill-column-indicator'."
  (interactive)
  (toggle-that-mode fci-mode))

(global-set-key (kbd "C-c v i") 'toggle-fill-column-indicator)

(use-package focus :ensure t)

(defun toggle-focus-mode ()
  "Toggle `focus-mode'."
  (interactive)
  (toggle-that-mode focus-mode))

(global-set-key (kbd "C-c v f") 'toggle-focus-mode)

(defun toggle-linenum-mode ()
  "Toggle `linum-mode'."
  (interactive)
  (toggle-that-mode global-linum-mode))

(global-set-key (kbd "C-c v l") 'toggle-linenum-mode)

(use-package rainbow-mode :ensure t)

(defun toggle-rainbow-mode ()
  "Toggle `rainbow-mode'."
  (interactive)
  (toggle-that-mode rainbow-mode))

(global-set-key (kbd "C-c v c") 'toggle-rainbow-mode)

(defun toggle-whitespace-mode-style ()
  "Toggle `whitespace-mode' style."
  (interactive)
  (let* ((min '(tabs tab-mark face trailing))
         (max '(tabs tab-mark spaces space-mark lines lines-tail newline newline-mark empty face trailing))
         (style (if (equal whitespace-active-style min) max min)))
    (setq whitespace-style style))
  (whitespace-turn-off)
  (whitespace-turn-on))

(global-set-key (kbd "C-c v w") 'toggle-whitespace-mode-style)

(provide 'init-theme)

;;; init-theme.el ends here
