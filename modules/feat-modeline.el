;;; feat-modeline.el --- Modeline feature -*- lexical-binding: t; -*-

;; Time-stamp: <2019-06-12 09:32:38>
;; Copyright (C) 2019 Pierre Lecocq

;;; Commentary:

;;; Code:

(when window-system
  (defgroup mymodeline nil "My modeline."
    :group 'faces)

  (defface mymodeline-blue-face
    '((t :foreground "DodgerBlue"))
    "Blue face for modeline.")

  (defface mymodeline-red-face
    '((t :foreground "#ff6666"))
    "Red face for modeline.")

  (setq-default mode-line-format
                '(" "
                  ;; Read only
                  (:eval (when buffer-read-only
                           (all-the-icons-faicon "lock" :height 0.9 :v-adjust 0 :face 'mymodeline-red-face)))
                  (:eval (when (not buffer-read-only)
                           (all-the-icons-icon-for-file (file-name-nondirectory buffer-file-name) :v-adjust 0)))
                  ;; Buffer name
                  (:eval (propertize " %b" 'face (if (and (not buffer-read-only)
                                                          (buffer-modified-p (current-buffer)))
                                                     'mymodeline-red-face
                                                   'mymodeline-blue-face)))
                  ;; Position
                  "    "
                  (:eval (all-the-icons-faicon "map-marker" :height 0.9 :v-adjust 0 :face 'mymodeline-blue-face))
                  " (%l,%c)"
                  ;; VC
                  "    "
                  (:eval (when vc-mode
                           (all-the-icons-faicon "code-fork" :height 0.9 :v-adjust 0 :face 'mymodeline-blue-face)))
                  (:eval vc-mode)
                  ;; Project
                  (:eval (when (projectile-project-name)
                           (format "@%s" (projectile-project-name))))
                  ;; Func
                  "    "
                  (:eval (when (which-function)
                           (all-the-icons-faicon "dot-circle-o" :height 0.9 :v-adjust 0 :face 'mymodeline-blue-face)))
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

(provide 'feat-modeline)

;;; feat-modeline.el ends here
