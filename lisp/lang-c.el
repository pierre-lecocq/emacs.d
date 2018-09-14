;;; lang-c.el --- C language support -*- lexical-binding: t; -*-

;; Time-stamp: <2018-09-12 10:27:55>
;; Copyright (C) 2018 Pierre Lecocq

;;; Commentary:

;;; Code:

(add-to-list 'auto-insert-alist
             '((c-mode . "C program") nil
               "/*\n"
               " * File: " (file-name-nondirectory buffer-file-name) "\n"
               " * Time-stamp: <>\n"
               " * Copyright (C) " (substring (current-time-string) -4) " " (user-full-name) "\n"
               " * Description: " _ "\n"
               " */\n\n"))

(use-package cc-mode :ensure t
  :config (setq gdb-many-windows t
                gdb-show-main t))

(use-package company-c-headers :ensure t
  :init (add-to-list 'company-backends 'company-c-headers))

(defun hook-c-mode ()
  "Hook for C mode."
  (c-set-offset 'case-label '+))

(add-hook 'c-mode-common-hook #'hook-c-mode)

;;; lang-c.el ends here
