;;; init-whitespace.el --- Emacs configuration - whitespace

;; Time-stamp: <2015-12-08 23:48:16>
;; Copyright (C) 2015 Pierre Lecocq

;;; Commentary:

;;; Code:

(require 'whitespace)

(setq highlight-tabs t
      show-trailing-whitespace t
      whitespace-line-column 80
      whitespace-style '(tabs tab-mark face lines-tail)
      whitespace-global-modes '(not org-mode web-mode))

(provide 'init-whitespace)

;;; init-whitespace.el ends here
