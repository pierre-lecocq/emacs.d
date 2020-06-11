;;; core-visuals.el --- Visuals -*- lexical-binding: t; -*-

;; Time-stamp: <2020-06-01 09:54:49>
;; Copyright (C) 2019 Pierre Lecocq

;;; Commentary:

;;; Code:

(toggle-frame-maximized)

(when (and window-system (eq system-type 'darwin))
  (setq frame-title-format nil
        ns-use-proxy-icon nil)
  (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
  (add-to-list 'default-frame-alist '(ns-appearance . dark)))

(setq mode-line-format (list " %* %b %l:%c"))

(defvar themes-candidates '(darkokai modus-operandi))

(defun switch-theme ()
  "Switch theme."
  (interactive)
  (let* ((cur (pop themes-candidates))
         (next (car themes-candidates)))
    (disable-theme cur)
    (load-theme next t)
    (setq themes-candidates (append themes-candidates `(,cur)))))

(global-set-key (kbd "C-c v t") 'switch-theme)

(use-package darkokai-theme :ensure t
  :config (load-theme 'darkokai t)
  :custom (darkokai-distinct-fringe-background nil)
  (darkokai-mode-line-padding 4)
  :custom-face (default ((t (:background "#191919"))))
  (fringe ((t (:background "#191919")))))

(use-package modus-operandi-theme :ensure t)

(use-package minions :ensure t
  :init (setq minions-mode-line-lighter "..."
              minions-mode-line-delimiters '("" . ""))
  :config (minions-mode 1))

(use-package window :no-require t
  :init (setq display-buffer-alist
              '(("\\*e?shell.*"
                 (display-buffer-in-side-window)
                 (window-height . 0.25)
                 (side . bottom)
                 (slot . 0))
                ("\\*\\(Backtrace\\|Warnings\\|Compile-Log\\|[Hh]elp\\|Messages\\|Flycheck errors\\)\\*"
                 (display-buffer-in-side-window)))))

(defun toggle-show-paren-mode-style ()
  "Toggle 'show-paren-mode' style."
  (interactive)
  (let* ((min 'parenthesis)
         (max 'expression)
         (style (if (equal show-paren-style min) max min)))
    (setq show-paren-style style)))

(global-set-key (kbd "C-c v p") 'toggle-show-paren-mode-style)

(use-package fill-column-indicator :ensure t)

(defun toggle-fill-column-indicator ()
  "Toggle 'fill-column-indicator'."
  (interactive)
  (fci-mode (if (bound-and-true-p fci-mode) -1 1)))

(global-set-key (kbd "C-c v i") 'toggle-fill-column-indicator)

(use-package highlight-indentation :ensure t)

(defun toggle-highlight-indentation ()
  "Toggle 'highlight-indentation'."
  (interactive)
  (highlight-indentation-mode (if (bound-and-true-p highlight-indentation-mode) -1 1)))

(global-set-key (kbd "C-c v h") 'toggle-highlight-indentation)

(defun toggle-linenum-mode ()
  "Toggle 'linum-mode'."
  (interactive)
  (global-linum-mode (if (bound-and-true-p global-linum-mode) -1 1)))

(global-set-key (kbd "C-c v l") 'toggle-linenum-mode)

(defun toggle-whitespace-mode-style ()
  "Toggle 'whitespace-mode' style."
  (interactive)
  (let* ((min '(tabs tab-mark face trailing))
         (max '(tabs tab-mark spaces space-mark lines lines-tail newline newline-mark empty face trailing))
         (style (if (equal whitespace-active-style min) max min)))
    (setq whitespace-style style))
  (whitespace-turn-off)
  (whitespace-turn-on))

(global-set-key (kbd "C-c v w") 'toggle-whitespace-mode-style)

(provide 'core-visuals)

;;; core-visuals.el ends here
