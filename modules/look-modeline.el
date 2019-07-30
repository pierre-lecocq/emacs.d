;;; look-modeline.el --- Modeline feature -*- lexical-binding: t; -*-

;; Time-stamp: <2019-07-30 12:02:30>
;; Copyright (C) 2019 Pierre Lecocq

;;; Commentary:

;;; Code:

(unless (boundp 'host-modeline-type)
  (defvar host-modeline-type 'default))

(cond ((eq host-modeline-type 'none)
       (setq-default mode-line-format nil))

      ((eq host-modeline-type 'simple)
       (setq-default mode-line-format
                     '(" %* "
                       (:eval (if (projectile-project-p)
                                  (concat "@" (projectile-project-name) "/" (replace-regexp-in-string (projectile-project-root) "" buffer-file-name))
                                (buffer-file-name)))
                       " [%l:%c %p] %m "
                       (:eval '(which-function-mode ("" which-func-format ""))))))

      ((eq host-modeline-type 'full)
       (defgroup host-modeline nil
         "My modeline"
         :group 'faces)

       (defface host-modeline-blue-face
         '((t :foreground "DodgerBlue"))
         "Blue face for modeline.")

       (defface host-modeline-red-face
         '((t :foreground "#ff6666"))
         "Red face for modeline")

       (setq-default mode-line-format
                     '(" "
                       ;; File icon or Read only
                       (:eval (if buffer-read-only
                                  (all-the-icons-faicon "lock" :height 0.8 :v-adjust 0 :face 'host-modeline-red-face)
                                (all-the-icons-icon-for-file (file-name-nondirectory buffer-file-name) :height 0.8 :v-adjust 0)))
                       ;; Buffer name
                       (:eval (propertize " %b" 'face (if (and (not buffer-read-only)
                                                               (buffer-modified-p (current-buffer)))
                                                          'host-modeline-red-face
                                                        'host-modeline-blue-face)))
                       ;; Position
                       "    "
                       (:eval (all-the-icons-faicon "map-marker" :height 0.8 :v-adjust 0 :face 'host-modeline-blue-face))
                       " (%l,%c)"
                       ;; VC
                       "    "
                       (:eval (when vc-mode
                                (all-the-icons-faicon "code-fork" :height 0.8 :v-adjust 0 :face 'host-modeline-blue-face)))
                       (:eval vc-mode)
                       ;; Project
                       (:eval (when (projectile-project-name)
                                (format "@%s" (projectile-project-name))))
                       ;; Func
                       "    "
                       (:eval (when (which-function)
                                (all-the-icons-faicon "dot-circle-o" :height 0.8 :v-adjust 0 :face 'host-modeline-blue-face)))
                       " "
                       (:eval (which-function))))

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
                           :underline nil)))

(provide 'look-modeline)

;;; look-modeline.el ends here
