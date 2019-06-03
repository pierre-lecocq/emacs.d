;;; lang-http.el --- HTTP language support -*- lexical-binding: t; -*-

;; Time-stamp: <2019-06-03 11:46:23>
;; Copyright (C) 2019 Pierre Lecocq

;;; Commentary:

;;; Code:

(use-package restclient :ensure t
  :mode (("\\.http\\'" . restclient-mode)
         ("\\.rest\\'" . restclient-mode)))

(provide 'lang-http)

;;; lang-http.el ends here
