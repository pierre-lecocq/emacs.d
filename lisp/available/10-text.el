;;; 10-text.el --- Text

;; Time-stamp: <2016-08-01 22:19:32>
;; Copyright (C) 2016 Pierre Lecocq

;;; Commentary:

;;; Code:

(use-package flycheck :ensure t
  :config (global-flycheck-mode t))

(use-package org :ensure t
  :init (progn
          (setq org-hide-leading-stars t
                org-hide-emphasis-markers t
                org-fontify-done-headline t
                org-src-fontify-natively t)))

(use-package markdown-mode :ensure t)

(use-package yaml-mode :ensure t)

(defun hook-text-mode ()
  "Hook  for Text mode."
  (make-local-variable 'linum-format)
  (linum-mode (if (< (buffer-size) (* 9999 80)) 1 -1))
  (electric-indent-local-mode -1)
  (setq linum-format " %d "))

(add-hook 'text-mode-hook #'hook-text-mode)

;;; 10-text.el ends here
