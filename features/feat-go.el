;;; feat-go.el --- Go feature -*- lexical-binding: t; -*-

;; Time-stamp: <2019-05-31 13:34:43>
;; Copyright (C) 2019 Pierre Lecocq

;;; Commentary:

;;; Code:

(use-package exec-path-from-shell :ensure t
  :if (memq window-system '(mac ns))
  :init (progn (exec-path-from-shell-initialize)
               (exec-path-from-shell-copy-env "GOPATH")))

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

(provide 'feat-go)

;;; feat-go.el ends here
