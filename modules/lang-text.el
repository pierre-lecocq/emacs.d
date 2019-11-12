;;; lang-text.el --- Text language support -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(use-package dockerfile-mode :ensure t)

(use-package dotenv-mode :ensure t
  :mode "\\.env\\..*\\'")

(use-package json-mode :ensure t)

(use-package markdown-mode :ensure t)

(use-package markdown-toc :ensure t
  :after (markdown-mode))

(use-package terraform-mode :ensure t)

(use-package toml-mode :ensure t :defer t)

(use-package yaml-mode :ensure t
  :mode "\\.ya?ml\\'")

(defun hook-text-mode ()
  "Hook for Text mode."
  (electric-indent-local-mode -1))

(add-hook 'text-mode-hook #'hook-text-mode)

(defun markdown-insert-timestamp-header (arg)
  "Insert timstamp ARG level header in a mardown document."
  (interactive "P")
  (let ((arg (if arg arg 1)))
    (insert (format "%s %s\n" (make-string arg ?#) (current-time-string)))))

(provide 'lang-text)

;;; lang-text.el ends here
