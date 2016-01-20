;;; config-whitespace.el --- Emacs configuration - whitespace

;; Time-stamp: <2016-01-20 09:09:01>
;; Copyright (C) 2015 Pierre Lecocq

;;; Commentary:

;;; Code:

(require 'whitespace)

(setq highlight-tabs t
      show-trailing-whitespace t
      whitespace-line-column 80
      whitespace-style '(tabs tab-mark face lines-tail)
      whitespace-global-modes '(not org-mode web-mode))

(provide 'config-whitespace)

;;; config-whitespace.el ends here
