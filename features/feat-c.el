;;; feat-c.el --- C feature -*- lexical-binding: t; -*-

;; Time-stamp: <2019-05-31 11:42:30>
;; Copyright (C) 2019 Pierre Lecocq

;;; Commentary:

;;; Code:

(use-package cc-mode :ensure t
  :config (setq gdb-many-windows t
                gdb-show-main t))

(use-package company-c-headers :ensure t
  :init (progn
          (add-to-list 'company-backends 'company-cmake)
          (add-to-list 'company-backends 'company-c-headers)))

(defun hook-c-mode ()
  "Hook for C mode."
  (c-set-offset 'case-label '+))

(add-hook 'c-mode-common-hook #'hook-c-mode)

(provide 'feat-c)

;;; feat-c.el ends here
