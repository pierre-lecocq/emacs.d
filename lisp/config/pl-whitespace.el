;;; pl-whitespace.el --- Emacs configuration - whitespace

;; Time-stamp: <2016-02-29 00:03:52>
;; Copyright (C) 2015 Pierre Lecocq

;;; Commentary:

;;; Code:

(require 'whitespace)

(setq highlight-tabs t
      show-trailing-whitespace t
      whitespace-line-column 80
      whitespace-style '(tabs tab-mark face lines-tail)
      whitespace-global-modes '(not org-mode web-mode))

(provide 'pl-whitespace)

;;; pl-whitespace.el ends here
