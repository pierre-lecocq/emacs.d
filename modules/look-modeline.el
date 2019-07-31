;;; look-modeline.el --- Modeline feature -*- lexical-binding: t; -*-

;; Time-stamp: <2019-07-31 23:27:01>
;; Copyright (C) 2019 Pierre Lecocq

;;; Commentary:

;;; Code:

(unless (boundp 'host-modeline-type)
  (defvar host-modeline-type 'default))

(cond ((eq host-modeline-type 'none)
       (setq-default mode-line-format nil))

      ((eq host-modeline-type 'simple)
       (setq-default mode-line-format
                     '(" %* %l:%c "
                       (:eval (all-the-icons-icon-for-file (file-name-nondirectory buffer-file-name) :height 0.6 :v-adjust 0))
                       " "
                       (:eval (if (projectile-project-p)
                                  (concat "@" (projectile-project-name) "/" (replace-regexp-in-string (projectile-project-root) "" buffer-file-name))
                                (buffer-file-name)))
                       " "
                       (:eval '(which-function-mode ("" which-func-format "")))
                       (:eval (replace-regexp-in-string "Git:" "" vc-mode))))))

(provide 'look-modeline)

;;; look-modeline.el ends here
