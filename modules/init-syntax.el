;;; init-syntax.el --- Syntax init -*- lexical-binding: t; -*-

;; Time-stamp: <2019-08-27 10:00:32>
;; Copyright (C) 2019 Pierre Lecocq

;;; Commentary:

;;; Code:

(use-package flycheck :ensure t
  :bind (("C-c s e" . flycheck-list-errors))
  :hook (prog-mode . flycheck-mode))

(provide 'init-syntax)

;;; init-syntax.el ends here
