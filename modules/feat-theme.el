;;; feat-theme.el --- Theme feature -*- lexical-binding: t; -*-

;; Time-stamp: <2019-06-03 22:59:20>
;; Copyright (C) 2019 Pierre Lecocq

;;; Commentary:

;;; Code:

(when (display-graphic-p)
  ;; (toggle-frame-maximized)
  (toggle-frame-fullscreen))

(set-background-color "#1a1a1a")
(set-foreground-color "#fafafa")
(set-face-background 'region "DodgerBlue")
(set-face-foreground 'font-lock-comment-face "#6a6a6a")
(set-face-foreground 'font-lock-string-face "#ff6666")
(set-face-background hl-line-face "#2a2a2a")
(set-face-background 'vertical-border "#4a4a4a")
(set-face-foreground 'vertical-border (face-background 'vertical-border))

(set-face-attribute 'mode-line nil
                    :background "#2a2a2a"
                    :foreground "#dadada"
                    :box '(:line-width 3 :color "#2a2a2a")
                    :overline nil
                    :underline nil)

(set-face-attribute 'mode-line-inactive nil
                    :background "#3a3a3a"
                    :foreground "#7a7a7a"
                    :box '(:line-width 3 :color "#3a3a3a")
                    :overline nil
                    :underline nil)

(setq-default left-fringe-width 20
              right-fringe-width 20)

(set-face-attribute 'fringe nil
                    :foreground (face-foreground 'default)
                    :background (face-background 'default))

(when (and window-system
           (eq system-type 'darwin)
           (not (version< emacs-version "26.1")))
  (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
  (add-to-list 'default-frame-alist '(ns-appearance . dark)))

(use-package all-the-icons :ensure t) ;; Run `M-x all-the-icons-install-fonts'

(use-package all-the-icons-dired :ensure t
  :custom-face (all-the-icons-dired-dir-face ((t (:foreground nil))))
  :hook (dired-mode . all-the-icons-dired-mode))

(provide 'feat-theme)

;;; feat-theme.el ends here
