;;; lang-makefile.el --- Makefile language support -*- lexical-binding: t; -*-

;; Time-stamp: <2018-09-12 10:28:55>
;; Copyright (C) 2018 Pierre Lecocq

;;; Commentary:

;;; Code:

(defun hook-makefile-mode ()
  "Hook for Makefile mode."
  (whitespace-toggle-options '(tabs))
  (setq indent-tabs-mode t))

(add-hook 'makefile-mode-hook #'hook-makefile-mode)

;;; lang-makefile.el ends here
