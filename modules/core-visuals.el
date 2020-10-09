;;; core-visuals.el --- Visuals -*- lexical-binding: t; -*-

;; Time-stamp: <2020-09-02 12:46:15>
;; Copyright (C) 2019 Pierre Lecocq

;;; Commentary:

;;; Code:

(toggle-frame-maximized)

(when (and window-system (eq system-type 'darwin))
  (setq frame-title-format nil
        ns-use-proxy-icon nil
        frame-resize-pixelwise t)
  (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
  (add-to-list 'default-frame-alist '(ns-appearance . dark)))

;; Modeline

(defvar my-mode-line-format
  '(
    ;; Buffer status
    " %* "
    ;; Project
    (:eval (when (and (fboundp 'projectile-project-p)
                      (projectile-project-p)
                      (or (derived-mode-p 'prog-mode)
                          (derived-mode-p 'text-mode)))
             (concat (propertize (projectile-project-name)
                                 'face 'bold)
                     "/")))
    ;; Buffer name, line, column, position
    "%b (%l:%c %p) "
    ;; Which func
    (:eval (when (derived-mode-p 'prog-mode)
             '(which-func-mode ("" which-func-format ""))))
    ;; Git branch
    (:eval (when vc-mode
             (propertize vc-mode
                         ;;(string-trim (replace-regexp-in-string "Git\.?" ":" vc-mode))
                         'face 'bold)))
    ;; Spacing
    (:eval (propertize " "
                       'display `((space :align-to (- (+ right right-fringe right-margin)
                                                      ,(+ 2 (string-width mode-name)))))))
    ;; Major mode
    (:eval (propertize "%m" 'face 'bold))))

(setq-default mode-line-format my-mode-line-format)

;; Theme

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

;; Others

;; (use-package minions :ensure t
;;   :init (setq minions-mode-line-lighter "..."
;;               minions-mode-line-delimiters '("" . ""))
;;   :config (minions-mode 1))

(use-package window :no-require t
  :init (setq display-buffer-alist
              '(("\\*\\(e?shell\\|vterm\\).*"
                 (display-buffer-in-child-frame))
                ("\\*\\(Backtrace\\|Warnings\\|Compile-Log\\|[Hh]elp\\|Messages\\|Flycheck errors\\)\\*"
                 (display-buffer-in-side-window)
                 (window-height . 0.25)
                 (side . bottom)
                 (slot . 0)))))

;; Toggles

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

(defun toggle-line-numbers-mode ()
  "Toggle 'line-numbers-mode'."
  (interactive)
  (global-display-line-numbers-mode (if (bound-and-true-p global-display-line-numbers-mode) -1 1)))

(global-set-key (kbd "C-c v l") 'toggle-line-numbers-mode)

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

(defun toggle-modeline ()
  "Toggle the modeline."
  (interactive)
  (setq-default mode-line-format
                (if mode-line-format
                    nil
                  my-mode-line-format)))

(global-set-key (kbd "C-c v m") 'toggle-modeline)

(provide 'core-visuals)

;;; core-visuals.el ends here
