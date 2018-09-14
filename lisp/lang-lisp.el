;;; lang-lisp.el --- Lisp language support -*- lexical-binding: t; -*-

;; Time-stamp: <2018-09-12 23:20:17>
;; Copyright (C) 2018 Pierre Lecocq

;;; Commentary:

;;; Code:

(add-to-list 'auto-insert-alist
             '((lisp-mode . "Lisp program") nil
               ";;;; " (file-name-nondirectory buffer-file-name) "\n\n"
               ";; Time-stamp: <>\n"
               ";; Copyright (C) " (substring (current-time-string) -4) " " (user-full-name) "\n\n"))

(use-package slime-company :ensure t :defer t)

(use-package slime :ensure t
  :after (:all slime-company)
  :mode (("\\.lisp'"    . lisp-mode) ;; enables lazy loading in place of :defer
         ("\\.lsp'"     . lisp-mode)
         ("\\.cl'"      . lisp-mode)
         ("\\.asd'"     . lisp-mode)
         ("\\.fasl'"    . lisp-mode))
  :init (setq inferior-lisp-program
              (if (eq system-type 'darwin)
                  "/usr/local/bin/sbcl --noinform"
                "sbcl --noinform"))
  :config (slime-setup '(slime-company)))

(defun hook-lisp-mode ()
  "Hook for Lisp mode."
  (message "LISP MODE hook!")
  (global-prettify-symbols-mode 1)
  (slime-mode t)
  (let ((helper-file (expand-file-name "~/quicklisp/slime-helper.el")))
    (if (file-exists-p helper-file)
        (load helper-file)
      (warn "(ql:quickload \"quicklisp-slime-helper\") must be run in quicklisp before"))))

(add-hook 'lisp-mode-hook #'hook-lisp-mode)

(defun hook-inferior-lisp-mode ()
  "Hook for inferior Lisp  mode."
  (inferior-slime-mode t))

(add-hook 'inferior-lisp-mode-hook #'hook-inferior-lisp-mode)

;;; lang-lisp.el ends here
