;;; pl-theme.el --- Emacs configuration - theme

;; Time-stamp: <2016-03-17 21:38:32>
;; Copyright (C) 2016 Pierre Lecocq

;;; Commentary:

;;; Code:

;; (use-package darkmine-theme :ensure t)
(use-package tao-theme :ensure t)

(load-file "~/src/switch-theme/switch-theme.el")
(require 'switch-theme)

(setq switch-theme-cache-filename (concat files-dir "theme"))
;; (setq switch-theme-list '(("darkmine") ("whiteboard")))
(setq switch-theme-list '(("tao-yin") ("tao-yang")))

(switch-theme)

(provide 'pl-theme)

;;; pl-theme.el ends here
