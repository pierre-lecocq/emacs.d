;;; feat-syntax.el --- Syntax support -*- lexical-binding: t; -*-

;; Time-stamp: <2018-09-12 10:27:15>
;; Copyright (C) 2018 Pierre Lecocq

;;; Commentary:

;;; Code:

(use-package flycheck :ensure t :diminish flycheck-mode
  :bind (("<f8>" . flycheck-list-errors))
  :hook (prog-mode . flycheck-mode))

;;; feat-syntax.el ends here
