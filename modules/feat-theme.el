;;; feat-theme.el --- Theme feature -*- lexical-binding: t; -*-

;; Time-stamp: <2019-06-11 16:44:59>
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

;; Modeline

(when window-system
  (defgroup mytheme nil "My theme."
    :group 'faces)

  (defface modeline-blue-face
    '((t :foreground "DodgerBlue"))
    "Blue face for modeline.")

  (defface modeline-yellow-face
    '((t :foreground "yellow"))
    "Yellow face for modeline.")

  (defface modeline-red-face
    '((t :foreground "#ff6666"))
    "Red face for modeline.")

  (setq-default mode-line-format
                '(" "
                  ;; Read only
                  (:eval (when buffer-read-only
                           (all-the-icons-faicon "lock" :height 0.9 :v-adjust 0 :face 'modeline-red-face)))
                  ;; Modified
                  (:eval (when (not buffer-read-only)
                           (all-the-icons-faicon "file" :height 0.9 :v-adjust 0 :face (if (buffer-modified-p (current-buffer))
                                                                                          'modeline-red-face
                                                                                        'modeline-blue-face))))
                  ;; Buffer name
                  " %b"
                  ;; Position
                  "    "
                  (:eval (all-the-icons-faicon "map-marker" :height 0.9 :v-adjust 0 :face 'modeline-blue-face))
                  " (%l,%c)"
                  ;; Mode
                  "    "
                  (:eval (all-the-icons-faicon "code" :height 0.9 :v-adjust 0 :face 'modeline-blue-face))
                  " %m"
                  ;; VC
                  "    "
                  (:eval (when vc-mode
                           (all-the-icons-faicon "code-fork" :height 0.9 :v-adjust 0 :face 'modeline-yellow-face)))
                  (:eval vc-mode)
                  ;; Project
                  (:eval (when (projectile-project-name)
                           (format "@%s" (projectile-project-name))))
                  ;; Func
                  "    "
                  (:eval (when (which-function)
                           (all-the-icons-faicon "dot-circle-o" :height 0.9 :v-adjust 0 :face 'modeline-blue-face)))
                  " "
                  (:eval (which-function)))))

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

(provide 'feat-theme)

;;; feat-theme.el ends here
