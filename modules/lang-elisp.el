;;; lang-elisp.el --- Emacs lisp -*- lexical-binding: t; -*-

;; Time-stamp: <2020-05-15 15:21:24>
;; Copyright (C) 2019 Pierre Lecocq

;;; Commentary:

;;; Code:

(use-package eros :ensure t)

(defun hook-emacs-lisp-mode ()
  "Hook for emacs-lisp mode."
  (eros-mode)
  (eldoc-mode))

(add-hook 'emacs-lisp-mode-hook #'hook-emacs-lisp-mode)
(add-hook 'eval-expression-minibuffer-setup-hook #'eldoc-mode)

(provide 'lang-elisp)

;;; lang-elisp.el ends here
