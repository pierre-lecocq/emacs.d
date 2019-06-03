;;; lang-makefile.el --- Makefile language support -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(defun hook-makefile-mode ()
  "Hook for Makefile mode."
  (whitespace-toggle-options '(tabs))
  (setq indent-tabs-mode t))

(add-hook 'makefile-mode-hook #'hook-makefile-mode)

(provide 'lang-makefile)

;;; lang-makefile.el ends here
