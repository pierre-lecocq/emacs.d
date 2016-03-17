;;; pl-theme.el --- Emacs configuration - theme

;; Time-stamp: <2016-03-17 15:45:38>
;; Copyright (C) 2016 Pierre Lecocq

;;; Commentary:

;;; Code:

(use-package darkmine-theme :ensure t)

(load-file "~/src/switch-theme/switch-theme.el")
(require 'switch-theme)

(setq switch-theme-list '(("darkmine")
                          ("whiteboard")))

(switch-theme)

(provide 'pl-theme)

;;; pl-theme.el ends here
