;;; lang-text.el --- Text files -*- lexical-binding: t; -*-

;; Time-stamp: <2020-05-20 17:07:48>
;; Copyright (C) 2019 Pierre Lecocq

;;; Commentary:

;;; Code:

(use-package dockerfile-mode :ensure t)

(use-package dotenv-mode :ensure t
  :mode "\\.env\\..*\\'")

(use-package json-mode :ensure t)

(use-package markdown-mode :ensure t
  :mode (("README\\.md\\'" . gfm-mode))
  :init (setq markdown-enable-wiki-links t
              markdown-italic-underscore t
              markdown-asymmetric-header t
              markdown-make-gfm-checkboxes-buttons t
              markdown-gfm-uppercase-checkbox t
              markdown-fontify-code-blocks-natively t)
  :bind ("C-c m p" . markdown-view-html))

(defun markdown-view-html ()
  "View markdown in HTML."
  (interactive)
  (xwidget-webkit-browse-url
   (concat "file://" (markdown-export))))

(use-package yaml-mode :ensure t
  :mode "\\.ya?ml\\'")

(defun hook-text-mode ()
  "Hook for Text mode."
  (electric-indent-local-mode -1))

(add-hook 'text-mode-hook #'hook-text-mode)

(provide 'lang-text)

;;; lang-text.el ends here
