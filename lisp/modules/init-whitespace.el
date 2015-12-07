;;; init-whitespace.el --- Emacs configuration - whitespace

;; Time-stamp: <2015-12-07 22:19:02>
;; Copyright (C) 2015 Pierre Lecocq

;;; Commentary:

;;; Code:

(setq highlight-tabs t
      show-trailing-whitespace t
      whitespace-line-column 80
      whitespace-style '(tabs tab-mark face)
      whitespace-global-modes '(not org-mode web-mode))

(provide 'init-whitespace)

;;; init-whitespace.el ends here
