;;; lang-text.el --- Text languages support -*- lexical-binding: t; -*-

;; Time-stamp: <2018-09-12 10:43:59>
;; Copyright (C) 2018 Pierre Lecocq

;;; Commentary:

;;; Code:

(add-to-list 'auto-insert-alist
             '((org-mode . "Org mode") nil
               "#+TITLE: " _ "\n"
               "#+AUTHOR: " (user-full-name) "\n"
               "#+DATE: " (current-time-string) "\n"
               "#+STARTUP: showall\n\n"))

(use-package dockerfile-mode :ensure t :defer t)

(use-package terraform-mode :ensure t :defer t)

(use-package json-mode :ensure t :defer t)

(use-package org :ensure t :defer t :pin "org"
  :init (setq org-hide-leading-stars t
              org-hide-emphasis-markers t
              org-fontify-done-headline t
              org-src-fontify-natively t))

(use-package markdown-mode :ensure t :defer t)

(use-package toml-mode :ensure t :defer t)

(use-package yaml-mode :ensure t
  :mode "\\.ya?ml\\'")

(defun hook-text-mode ()
  "Hook for Text mode."
  (electric-indent-local-mode -1))

(add-hook 'text-mode-hook #'hook-text-mode)

;;; lang-text.el ends here
