;;; config-text.el --- Emacs configuration - text

;; Time-stamp: <2016-01-20 09:08:43>
;; Copyright (C) 2015 Pierre Lecocq

;;; Commentary:

;;; Code:

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
  (linum-mode 1)
  (make-local-variable 'linum-format)
  (setq linum-format " %d "))

(add-hook 'text-mode-hook #'hook-text-mode)

(provide 'config-text)

;;; config-text.el ends here