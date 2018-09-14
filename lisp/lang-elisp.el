;;; lang-elisp.el --- Emacs lisp language support -*- lexical-binding: t; -*-

;; Time-stamp: <2018-09-12 10:28:15>
;; Copyright (C) 2018 Pierre Lecocq

;;; Commentary:

;;; Code:

(add-to-list 'auto-insert-alist
             '((emacs-lisp-mode . "Emacs lisp program") nil
               ";;; " (file-name-nondirectory buffer-file-name) " --- " _ " -*- lexical-binding: t; -*-\n\n"
               ";; Time-stamp: <>\n"
               ";; Copyright (C) " (substring (current-time-string) -4) " " (user-full-name) "\n\n"
               ";;; Commentary:\n\n"
               ";;; Code:\n\n"
               ";;; " (file-name-nondirectory buffer-file-name) " ends here\n"))

(use-package eros :ensure t)

(defun hook-emacs-lisp-mode ()
  "Hook for emacs-lisp mode."
  (eros-mode)
  (eldoc-mode)
  (global-prettify-symbols-mode 1))

(add-hook 'emacs-lisp-mode-hook #'hook-emacs-lisp-mode)
(add-hook 'eval-expression-minibuffer-setup-hook #'eldoc-mode)

;;; lang-elisp.el ends here
