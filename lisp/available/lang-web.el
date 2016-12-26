;;; lang-web.el --- Web

;; Time-stamp: <2016-12-26 21:10:46>
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

(use-package web-mode)

(provide 'lang-web)

;;; lang-web.el ends here
