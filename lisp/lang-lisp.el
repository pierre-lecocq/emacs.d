;;; lang-lisp.el --- Lisp language support -*- lexical-binding: t; -*-

;; Time-stamp: <2019-02-13 23:34:51>
;; Copyright (C) 2018 Pierre Lecocq

;;; Commentary:

;;; Code:

(setq inferior-lisp-program
      (if (eq system-type 'darwin)
          "/usr/local/bin/sbcl"
        "sbcl"))

(add-to-list 'auto-insert-alist
             '((lisp-mode . "Lisp program") nil
               ";;;; " (file-name-nondirectory buffer-file-name) "\n\n"
               ";; Time-stamp: <>\n"
               ";; Copyright (C) " (substring (current-time-string) -4) " " (user-full-name) "\n\n"))

(defun hook-lisp-mode ()
  "Hook for Lisp mode."
  ;; (global-prettify-symbols-mode 1)
  (slime-mode t)
  (let ((helper-file (expand-file-name "~/quicklisp/slime-helper.el")))
    (if (file-exists-p helper-file)
        (load helper-file)
      (warn "(ql:quickload \"quicklisp-slime-helper\") must be run in quicklisp before"))))

(defun hook-inferior-lisp-mode ()
  "Hook for inferior Lisp  mode."
  (inferior-slime-mode t))

(use-package slime-company :ensure t :defer t)

(use-package slime :ensure t
  :after (:all slime-company)
  :mode (("\\.lisp'"    . lisp-mode)
         ("\\.lsp'"     . lisp-mode)
         ("\\.cl'"      . lisp-mode)
         ("\\.asd'"     . lisp-mode)
         ("\\.fasl'"    . lisp-mode))
  :config (slime-setup '(slime-company))
  :init (setq slime-contribs '(slime-fancy))
  :hook ((lisp-mode . hook-lisp-mode)
         (inferior-lisp-mode . hook-inferior-lisp-mode)))

;;; lang-lisp.el ends here
