;;; module-syntax.el --- Syntax module -*- lexical-binding: t; -*-

;; Time-stamp: <2019-06-03 11:40:12>
;; Copyright (C) 2019 Pierre Lecocq

;;; Commentary:

;;; Code:

(use-package flycheck :ensure t
  :bind (("<f11>" . flycheck-list-errors))
  :hook (prog-mode . flycheck-mode))

(provide 'module-syntax)

;;; module-syntax.el ends here
