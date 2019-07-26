;;; look-theme.el --- Theme feature -*- lexical-binding: t; -*-

;; Time-stamp: <2019-07-26 09:00:49>
;; Copyright (C) 2019 Pierre Lecocq

;;; Commentary:

;;; Code:

(unless (boundp 'host-frame-type)
  (defvar host-frame-type 'maximized))

(cond ((eq host-frame-type 'maximized)
       (toggle-frame-maximized))
      ((eq host-frame-type 'fullscreen)
       (if (eq system-type 'darwin)
           (toggle-frame-fullscreen)
         (toggle-frame-maximized))))

(when (and window-system
           (eq system-type 'darwin)
           (not (version< emacs-version "26.1")))
  (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
  (add-to-list 'default-frame-alist '(ns-appearance . dark)))

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

(provide 'look-theme)

;;; look-theme.el ends here
