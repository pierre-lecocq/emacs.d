;;; lang-makefile.el --- Lang-Makefile -*- lexical-binding: t; -*-

;; Time-stamp: <2020-09-02 16:50:42>
;; Copyright (C) 2019 Pierre Lecocq

;;; Commentary:

;;; Code:

(defun hook-makefile-mode ()
  "Hook for Makefile mode."
  (whitespace-toggle-options '(tabs tab-mark))
  (setq indent-tabs-mode t))

(add-hook 'makefile-mode-hook #'hook-makefile-mode)
(add-to-list 'auto-mode-alist '("Makefile.*\\'" . makefile-mode))

(provide 'lang-makefile)

;;; lang-makefile.el ends here
