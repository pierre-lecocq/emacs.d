;;; init-theme.el --- Theme init -*- lexical-binding: t; -*-

;; Time-stamp: <2019-10-01 14:20:27>
;; Copyright (C) 2019 Pierre Lecocq

;;; Commentary:

;;; Code:

;; Themes

(load-theme 'dark t)

(defvar themes-candidates '(dark light))

(defun switch-theme ()
  "Switch theme."
  (interactive)
  (let* ((cur (pop themes-candidates))
         (next (car themes-candidates)))
    (disable-theme cur)
    (load-theme next t)
    (setq themes-candidates (append themes-candidates `(,cur)))))

(global-set-key (kbd "C-c v t") 'switch-theme)

;; Modeline

(setq-default mode-line-format
              '(" "
                ;; Buffer status
                "%*"
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
                                   'face 'bold
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

;; Frame

(if (eq system-type 'darwin)
    (toggle-frame-fullscreen)
  (toggle-frame-maximized))

(when (and window-system
           (eq system-type 'darwin)
           (not (version< emacs-version "26.1")))
  (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
  (add-to-list 'default-frame-alist '(ns-appearance . dark)))

;; Fringe

(setq-default left-fringe-width 20
              right-fringe-width 20)

(defun set-padding (padd)
  "Add PADD padding to the current frame."
  (interactive (list (string-to-number (read-from-minibuffer "Padding: "))))
  (set-frame-parameter (selected-frame) 'internal-border-width (or padd 0))
  (redraw-display))

;; Icons

(use-package all-the-icons :ensure t) ;; Run `M-x all-the-icons-install-fonts'

(use-package all-the-icons-dired :ensure t
  :hook (dired-mode . all-the-icons-dired-mode))

;; Visual help toggles

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

(global-set-key (kbd "C-c v b") 'toggle-rainbow-mode)

(use-package centered-window :ensure t)

(defun toggle-centered-window-mode ()
  "Toggle `centered-window-mode'."
  (interactive)
  (toggle-that-mode centered-window-mode))

(global-set-key (kbd "C-c v c") 'toggle-centered-window-mode)

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
