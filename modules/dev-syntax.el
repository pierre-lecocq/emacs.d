;;; dev-syntax.el --- Syntax -*- lexical-binding: t; -*-

;; Time-stamp: <2020-05-15 15:17:38>
;; Copyright (C) 2019 Pierre Lecocq

;;; Commentary:

;;; Code:

(use-package flycheck :ensure t
  :hook (prog-mode . flycheck-mode))

(provide 'dev-syntax)

;;; dev-syntax.el ends here
