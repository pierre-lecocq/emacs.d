;;; init-syntax.el --- Syntax init -*- lexical-binding: t; -*-

;; Time-stamp: <2019-08-22 15:20:08>
;; Copyright (C) 2019 Pierre Lecocq

;;; Commentary:

;;; Code:

(use-package flycheck :ensure t
  :bind (("<f5>" . flycheck-list-errors))
  :hook (prog-mode . flycheck-mode))

(provide 'init-syntax)

;;; init-syntax.el ends here
