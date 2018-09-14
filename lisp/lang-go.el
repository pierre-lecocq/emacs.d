;;; lang-go.el --- Go language support -*- lexical-binding: t; -*-

;; Time-stamp: <2018-09-12 23:19:29>
;; Copyright (C) 2018 Pierre Lecocq

;;; Commentary:

;;; Code:

(use-package exec-path-from-shell :ensure t
  :if (memq window-system '(mac ns))
  :init (progn (exec-path-from-shell-initialize)
               (exec-path-from-shell-copy-env "GOPATH")))

(use-package go-eldoc :ensure t :defer t)

(use-package go-mode :ensure t :defer t
  :after (:all go-eldoc))

(defun hook-go-mode ()
  "Hook for Go mode."
  (go-eldoc-setup)
  (add-hook 'before-save-hook 'gofmt-before-save)
  (set (make-local-variable 'company-backends) '(company-go))
  (setq whitespace-style '(spaces space-mark face)))

(add-hook 'go-mode-hook #'hook-go-mode)

;;; lang-go.el ends here
