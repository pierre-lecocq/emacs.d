;;; lang-c.el --- C language support -*- lexical-binding: t; -*-

;; Time-stamp: <2019-08-27 19:00:15>
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
  (setq c-default-style "linux"
        c-basic-offset 4)
  (c-set-offset 'case-label '+))

(add-hook 'c-mode-common-hook #'hook-c-mode)

(provide 'lang-c)

;;; lang-c.el ends here
