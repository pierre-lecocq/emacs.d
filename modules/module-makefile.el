;;; module-makefile.el --- Makefile feature -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(defun hook-makefile-mode ()
  "Hook for Makefile mode."
  (whitespace-toggle-options '(tabs))
  (setq indent-tabs-mode t))

(add-hook 'makefile-mode-hook #'hook-makefile-mode)

(provide 'module-makefile)

;;; module-makefile.el ends here
