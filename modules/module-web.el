;;; module-web.el --- Web feature -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(use-package htmlize :ensure t)

(use-package scss-mode :ensure t)

(use-package web-mode :ensure t
  :mode (("\\.html?\\'" . web-mode)
         ("\\.erb\\'" . web-mode)
         ("\\.erubis\\'" . web-mode)))

(provide 'module-web)

;;; module-web.el ends here
