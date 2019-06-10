;;; lang-web.el --- Web language support -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(use-package htmlize :ensure t)

(use-package scss-mode :ensure t)

(use-package web-mode :ensure t
  :mode (("\\.html?\\'" . web-mode)
         ("\\.erb\\'" . web-mode)
         ("\\.erubis\\'" . web-mode)
         ("\\.ejs\\'" . web-mode)))

(provide 'lang-web)

;;; lang-web.el ends here
