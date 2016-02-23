;;; config-theme.el --- Emacs configuration - theme

;; Time-stamp: <2016-02-23 22:36:22>
;; Copyright (C) 2016 Pierre Lecocq

;;; Commentary:

;;; Code:

(defvar pl-theme-index 1)
(defvar pl-themes-list '("tango-dark" "tango"))

(defun pl-switch-theme ()
  "Switch theme between dark and light ones."
  (interactive)
  (disable-theme (intern (nth pl-theme-index pl-themes-list)))
  (setq pl-theme-index (if (= pl-theme-index 0) 1 0))
  (load-theme (intern (nth pl-theme-index pl-themes-list)) t))

;; (use-package darkmine-theme :ensure t
;;   :init (progn
;;           (setq pl-themes-list '("darkmine" "dichromacy"))
;;           (pl-switch-theme)))

(use-package tao-theme :ensure t
  :init (progn
          (setq pl-themes-list '("tao-yin" "tao-yang"))
          (pl-switch-theme)))

(provide 'config-theme)

;;; config-theme.el ends here
