;;; module-go.el --- Go feature -*- lexical-binding: t; -*-

;; Time-stamp: <2019-06-03 11:01:19>
;; Copyright (C) 2019 Pierre Lecocq

;;; Commentary:

;; - go get -u golang.org/x/lint/golint

;;; Code:

(eval-after-load 'exec-path-from-shell
  (exec-path-from-shell-copy-env "GOPATH"))

(use-package go-eldoc :ensure t :defer t)

(use-package go-mode :ensure t :defer t
  :init (eval-after-load 'company
          '(push 'company-go company-backends)))

(use-package company-go :ensure t :defer t)

(defun hook-go-mode ()
  "Hook for Go mode."
  (go-eldoc-setup)
  (add-hook 'before-save-hook 'gofmt-before-save))

(add-hook 'go-mode-hook #'hook-go-mode)

(provide 'module-go)

;;; module-go.el ends here
