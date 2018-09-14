;;; lang-http.el --- HTTP language support -*- lexical-binding: t; -*-

;; Time-stamp: <2018-09-12 10:28:43>
;; Copyright (C) 2018 Pierre Lecocq

;;; Commentary:

;;; Code:

(add-to-list 'auto-insert-alist
             '((restclient-mode . "REST client") nil
               "# -*- restclient -*-\n\n"))

(use-package restclient :ensure t
  :mode (("\\.http\\'" . restclient-mode)
         ("\\.rest\\'" . restclient-mode)))

;;; lang-http.el ends here
