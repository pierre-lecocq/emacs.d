;;; lang-web.el --- Web languages support -*- lexical-binding: t; -*-

;; Time-stamp: <2018-09-12 23:21:34>
;; Copyright (C) 2018 Pierre Lecocq

;;; Commentary:

;;; Code:

(use-package htmlize :ensure t :defer t)

(use-package scss-mode :ensure t :defer t)

(use-package web-mode :ensure t
  :mode (("\\.html?\\'" . web-mode)
         ("\\.erb\\'" . web-mode)
         ("\\.erubis\\'" . web-mode)))

;;; lang-web.el ends here
