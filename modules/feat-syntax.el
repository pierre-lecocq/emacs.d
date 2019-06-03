;;; feat-syntax.el --- Syntax feature -*- lexical-binding: t; -*-

;; Time-stamp: <2019-06-03 14:55:38>
;; Copyright (C) 2019 Pierre Lecocq

;;; Commentary:

;;; Code:

(use-package flycheck :ensure t
  :bind (("<f11>" . flycheck-list-errors))
  :hook (prog-mode . flycheck-mode))

(provide 'feat-syntax)

;;; feat-syntax.el ends here
