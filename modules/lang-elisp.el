;;; lang-elisp.el --- Elisp language support -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(use-package eros :ensure t)

(defun hook-emacs-lisp-mode ()
  "Hook for emacs-lisp mode."
  (eros-mode)
  (eldoc-mode)
  (diminish 'eldoc-mode))

(add-hook 'emacs-lisp-mode-hook #'hook-emacs-lisp-mode)
(add-hook 'eval-expression-minibuffer-setup-hook #'eldoc-mode)

(provide 'lang-elisp)

;;; lang-elisp.el ends here
