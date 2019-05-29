;;; text.el --- Text feature -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(use-package dockerfile-mode :ensure t)

(use-package dotenv-mode :ensure t
  :mode "\\.env\\..*\\'")

(use-package json-mode :ensure t)

(use-package markdown-mode :ensure t)

(use-package flymd :ensure t
  :config (setq flymd-output-directory "/tmp"
                flymd-close-buffer-delete-temp-files t)
  :bind (("C-c m p" . flymd-flyit)))

(use-package terraform-mode :ensure t)

(use-package toml-mode :ensure t :defer t)

(use-package yaml-mode :ensure t
  :mode "\\.ya?ml\\'")

(defun hook-text-mode ()
  "Hook for Text mode."
  (electric-indent-local-mode -1))

(add-hook 'text-mode-hook #'hook-text-mode)

(provide 'text)

;;; text.el ends here
