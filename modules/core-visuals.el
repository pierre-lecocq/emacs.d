;;; core-visuals.el --- Visuals -*- lexical-binding: t; -*-

;; Time-stamp: <2020-12-21 09:43:27>
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

;; Theme

(load-file (expand-file-name "themes/darquiche-theme.el" user-emacs-directory))
(load-file (expand-file-name "themes/lightish-theme.el" user-emacs-directory))

(load-theme 'darquiche t)

(use-package cycle-themes :ensure t
  :init (setq cycle-themes-theme-list '(darquiche lightish))
  :config (cycle-themes-mode))

(setq-default mode-line-format
              '(" "
                ;; Project
                (:eval (when (and (fboundp 'projectile-project-p)
                                  (projectile-project-p)
                                  (or (derived-mode-p 'prog-mode)
                                      (derived-mode-p 'text-mode)))
                         (concat "  " (propertize (projectile-project-name)
                                                  'face 'bold))))
                ;; Git branch
                (:eval (when vc-mode
                         (propertize (string-trim (replace-regexp-in-string "Git\.?" ":" vc-mode))
                                     'face 'bold)))
                ;; Buffer name
                "  "
                (:eval (propertize "%b"
                                   'face `(:foreground ,(if (buffer-modified-p)
                                                            "#cc0000"
                                                          (face-attribute 'font-lock-keyword-face :foreground)))
                                   'help-echo (format "%s - %s" (buffer-file-name) mode-name)))
                ;; Line, column, position
                "  (%l:%c %p)  "
                ;; Which func
                (:eval (when (derived-mode-p 'prog-mode)
                         '(which-func-mode ("" which-func-format ""))))
                (:eval (propertize " "
                                   'display `((space :align-to (- (+ right right-fringe right-margin)
                                                                  ,(+ 2 (string-width mode-name)))))))
                ;; Major mode
                (:eval (propertize "%m" 'face 'bold))))

(advice-add 'c-update-modeline :around #'ignore) ;; This prevents options and minor modes hints to be appended to major mode name (i.e c-toggle-comment-style)

;; Windows

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

(defun toggle-fill-column-indicator ()
  "Toggle 'display-fill-column-indicator'."
  (interactive)
  (global-display-fill-column-indicator-mode (if (bound-and-true-p global-display-fill-column-indicator-mode) -1 1)))

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

(provide 'core-visuals)

;;; core-visuals.el ends here
