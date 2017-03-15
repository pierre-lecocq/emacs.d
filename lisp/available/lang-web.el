;;; lang-web.el --- Web

;; Time-stamp: <2017-03-15 14:39:33>
;; Copyright (C) 2015 Pierre Lecocq

;;; Commentary:

;;; Code:

(use-package htmlize :ensure t)

(use-package js2-mode :ensure t)

(use-package json-mode :ensure t)

(use-package php-mode :ensure t)

(use-package php-extras :ensure t)

(use-package rainbow-mode :ensure t
  :diminish rainbow-mode)

(use-package restclient :ensure t)

(use-package web-mode :ensure t
  :mode (("\\.html?\\'" . web-mode)
         ("\\.erb\\'" . web-mode)
         ("\\.erubis\\'" . web-mode)))

(provide 'lang-web)

;;; lang-web.el ends here
