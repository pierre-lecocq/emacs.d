;;; lang-go.el --- Lang-Go -*- lexical-binding: t; -*-

;; Time-stamp: <2020-06-08 12:28:22>
;; Copyright (C) 2019 Pierre Lecocq

;;; Commentary:

;;; Code:

(eval-after-load 'exec-path-from-shell
  (exec-path-from-shell-copy-env "GOPATH"))

(use-package go-eldoc :ensure t :defer t)

(use-package go-mode :ensure t :defer t)

(use-package company-go :ensure t :defer t
  :after company
  :config (add-to-list 'company-backends 'company-go))

(defun hook-go-mode ()
  "Hook for Go mode."
  (go-eldoc-setup)
  (add-hook 'before-save-hook 'gofmt-before-save))

(add-hook 'go-mode-hook #'hook-go-mode)

(provide 'lang-go)

;;; lang-go.el ends here
