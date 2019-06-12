;;; feat-theme.el --- Theme feature -*- lexical-binding: t; -*-

;; Time-stamp: <2019-06-12 08:50:07>
;; Copyright (C) 2019 Pierre Lecocq

;;; Commentary:

;;; Code:

(when window-system
  ;; Default frame size
  (toggle-frame-fullscreen) ; or (toggle-frame-maximized)
  ;; Title bar on Mac
  (when (and(eq system-type 'darwin)
            (not (version< emacs-version "26.1")))
    (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
    (add-to-list 'default-frame-alist '(ns-appearance . dark))))

;; Icons

(use-package all-the-icons :ensure t) ;; Run `M-x all-the-icons-install-fonts'

(use-package all-the-icons-dired :ensure t
  :custom-face (all-the-icons-dired-dir-face ((t (:foreground nil))))
  :hook (dired-mode . all-the-icons-dired-mode))

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

(provide 'feat-theme)

;;; feat-theme.el ends here
