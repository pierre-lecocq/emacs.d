;;; feat-theme.el --- Theme -*- lexical-binding: t; -*-

;; Time-stamp: <2018-09-12 23:17:28>
;; Copyright (C) 2018 Pierre Lecocq

;;; Commentary:

;;; Code:

(use-package idle-highlight-mode :ensure t :diminish idle-highlight-mode
  :hook (prog-mode . idle-highlight-mode))

(use-package rainbow-mode :ensure t :diminish rainbow-mode
  :hook (prog-mode . rainbow-turn-on))

(use-package rainbow-delimiters :ensure t
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package darkokai-theme :ensure t
  :config (load-theme 'darkokai t)
  :init (progn (setq darkokai-mode-line-padding 1)
               (setq-default left-fringe-width 10
                             right-fringe-width 10)
               (when (and window-system
                          (eq system-type 'darwin)
                          (not (version< emacs-version "26.1")))
                 (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
                 (add-to-list 'default-frame-alist '(ns-appearance . dark)))))

(set-face-background 'region "DodgerBlue")
(set-face-foreground 'region "white")
(set-face-underline 'font-lock-warning-face "red")
(font-lock-add-keywords nil '(("\\<\\(FIXME\\|TODO\\|BUG\\)" 1 font-lock-warning-face t)))

(defun set-font-size (size)
  "Change font SIZE."
  (interactive "nFont size: ")
  (set-frame-font (format "Source Code Pro %d" size) nil t))

(set-font-size 12)

;;; feat-theme.el ends here
