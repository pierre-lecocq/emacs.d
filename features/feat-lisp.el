;;; feat-lisp.el --- Lisp feature -*- lexical-binding: t; -*-

;; Time-stamp: <2019-05-31 14:31:27>
;; Copyright (C) 2019 Pierre Lecocq

;;; Commentary:

;;; Code:

(use-package slime-company :ensure t :defer t)

(use-package slime :ensure t
  :mode (("\\.lisp'"    . lisp-mode)
         ("\\.lsp'"     . lisp-mode)
         ("\\.cl'"      . lisp-mode)
         ("\\.asd'"     . lisp-mode)
         ("\\.fasl'"    . lisp-mode))
  :config (slime-setup '(slime-company))
  :init (setq slime-contribs '(slime-fancy)))

(defun hook-lisp-mode ()
  "Hook for Lisp mode."
  (slime-mode t)
  (let ((helper-file (expand-file-name "~/quicklisp/slime-helper.el")))
    (if (file-exists-p helper-file)
        (load helper-file)
      (warn "(ql:quickload \"quicklisp-slime-helper\") must be run in quicklisp before")))
  (setq inferior-lisp-program (if (eq system-type 'darwin) "/usr/local/bin/sbcl" "sbcl")))

(defun hook-inferior-lisp-mode ()
  "Hook for inferior Lisp  mode."
  (inferior-slime-mode t))

(add-hook 'lisp-mode-hook #'hook-lisp-mode)
(add-hook 'inferior-lisp-mode-hook #'hook-inferior-lisp-mode)

(provide 'feat-lisp)

;;; feat-lisp.el ends here
