;;; lang-sql.el --- SQL language support -*- lexical-binding: t; -*-

;; Time-stamp: <2018-09-12 10:42:50>
;; Copyright (C) 2018 Pierre Lecocq

;;; Commentary:

;;; Code:

(use-package sqlup-mode :ensure t :defer t)

(use-package sql-indent :ensure t :defer t)

(defun hook-sql-mode ()
  "Hook for SQL mode."
  (sqlup-mode t)
  (toggle-truncate-lines t))

(add-hook 'sql-mode-hook #'hook-sql-mode)
(add-hook 'sql-interactive-mode-hook #'hook-sql-mode) ;; When connected to a server within Emacs

;;; lang-sql.el ends here
