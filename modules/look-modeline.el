;;; look-modeline.el --- Modeline feature -*- lexical-binding: t; -*-

;; Time-stamp: <2019-07-30 22:23:27>
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
                       (:eval '(which-function-mode ("" which-func-format "")))))))

(provide 'look-modeline)

;;; look-modeline.el ends here
